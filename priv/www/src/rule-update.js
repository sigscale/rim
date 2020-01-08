/**
 * @license
 * Copyright (c) 2020 The Polymer Project Authors. All rights reserved.
 * This code may only be used under the BSD style license found at http://polymer.github.io/LICENSE.txt
 * The complete set of authors may be found at http://polymer.github.io/AUTHORS.txt
 * The complete set of contributors may be found at http://polymer.github.io/CONTRIBUTORS.txt
 * Code distributed by Google as part of the polymer project is also
 * subject to an additional IP rights grant found at http://polymer.github.io/PATENTS.txt
 */

import { PolymerElement, html } from '@polymer/polymer/polymer-element.js';
import '@polymer/iron-ajax/iron-ajax.js';
import '@polymer/paper-dialog/paper-dialog.js';
import '@polymer/app-layout/app-toolbar/app-toolbar.js';
import '@polymer/paper-progress/paper-progress.js';
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-input/paper-textarea.js';
import '@polymer/paper-button/paper-button.js';
import './style-element.js';

class ruleUpdate extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="ruleUpdateModal" modal>
				<app-toolbar>
					<div main-title>Update Rule</div>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
					<paper-input
							label="Id"
							value="{{ruleId}}"
							disabled>
					</paper-input>
					<paper-input
							label="Rule"
							value="{{ruleRules}}"
							disabled>
					</paper-input>
					<paper-textarea
							label="Description"
							value="{{ruleDescription}}">
					</paper-textarea>
					<div class="buttons">
						<paper-button
								raised
								class="update-button"
								on-tap="_update">
							Update
						</paper-button>
						<paper-button
								class="cancel-button"
								on-tap="_cancel">
							Cancel
						</paper-button>
					</div>
			</paper-dialog>
			<iron-ajax
				id="ruleUpdateAjax"
				content-type="application/json-patch+json"
				loading="{{loading}}"
				on-loading-changed="_onLoadingChanged"
				on-response="_response"
				on-error="_error">
			</iron-ajax>
			<iron-ajax
				id="ruleDeleteAjax"
				content-type="application/json"
				loading="{{loading}}"
				on-response="_response"
				on-error="_error">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				value: false
			},
			activeItem: {
				type: Object,
				observer: '_activeItemChanged'
			},
			ruleId: {
				type: String
			},
			ruleDescription: {
				type: String
			},
			ruleRules: {
				type: Array
			}
		}
	}

	ready() {
		super.ready()
	}

	_activeItemChanged(item) {
		if(item) {
			this.ruleId = item.id;
			this.ruleDescription = item.description;
			this.ruleRules = item.rules;
			this.$.ruleUpdateModal.open();
		} else {
			this.ruleId = null;
			this.ruleDescription = null;
			this.rulerules = [];
		}
	}

	_cancel() {
		this.$.ruleUpdateModal.close();
		this.ruleId = null;
		this.ruleDescription = null;
		this.rulerules = [];
	}

	_update() {
		var ajax = this.$.ruleUpdateAjax;
		ajax.method = "PATCH";
		ajax.url = "/resourceInventoryManagement/v1/logicalResource/" + this.$.addRuleId.value;
		var ruleArr = new Array()
		var peeRule = new Object();
		if(this.rule.description) {
			peeRule.op = "add";
			peeRule.path = "/description";
			peeRule.value = this.rule.description;
		}
		ruleArr.push(peeRule);
		ajax.body = JSON.stringify(RuleArr);
		ajax.generateRequest();
	}

	_response() {
		this.$.ruleUpdateModal.close();
		document.body.querySelector('inventory-management').shadowRoot.getElementById('ruleList').shadowRoot.getElementById('ruleGrid').clearCache();
	}

	_error(event) {
		var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
		toast.text = event.detail.request.xhr.statusText;
		toast.open();
	}
}

window.customElements.define('rule-update', ruleUpdate);
