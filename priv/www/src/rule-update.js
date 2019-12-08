/**
 * @license
 * Copyright (c) 2019 The Polymer Project Authors. All rights reserved.
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
import '@polymer/paper-button/paper-button.js';
import './style-element.js';

class ruleUpdate extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="updateRuleModal" modal>
				<app-toolbar>
					<div main-title>Update Rule</div>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
					<paper-input
							id="addRuleId"
							label="Id"
							value="{{rule.ruleId}}"
							disabled>
					</paper-input>
					<paper-input
							id="addRule"
							label="Rule"
							value="{{rule.rules}}"
							disabled>
					</paper-input>
					<paper-input
							id="addRuleDesc"
							label="Description"
							value="{{rule.ruleDesc}}">
					</paper-input>
					<div class="buttons">
						<paper-button
								raised
								class="update-button"
								on-tap="_update">
							Update
						</paper-button>
						<paper-button
								class="cancel-button"
								dialog-dismiss>
							Cancel
						</paper-button>
					</div>
			</paper-dialog>
			<iron-ajax
				id="ruleUpdateAjax"
				content-type="application/json-patch+json"
				loading="{{loading}}"
				on-loading-changed="_onLoadingChanged"
				on-response="_ruleUpdateResponse"
				on-error="_ruleUpdateError">
			</iron-ajax>
			<iron-ajax
				id="ruleDeleteAjax"
				content-type="application/json"
				loading="{{loading}}"
				on-response="_ruleUpdateResponse"
				on-error="_ruleUpdateError">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				value: false
			},
			rule: {
				type: Object,
			}
		}
	}

	ready() {
		super.ready()
	}

	_update() {
		var ajax = this.$.ruleUpdateAjax;
		ajax.method = "PATCH";
		ajax.url = "/resourceInventoryManagement/v1/logicalResource/" + this.$.addRuleId.value;
		var RuleArr = new Array()
		var peeRule = new Object();
		if(this.$.addRuleDesc.value) {
			peeRule.op = "add";
			peeRule.path = "/description";
			peeRule.value = this.$.addRuleDesc.value
		}
		RuleArr.push(peeRule);
		ajax.body = JSON.stringify(RuleArr);
		ajax.generateRequest();
	}

	_ruleUpdateResponse() {
		var shell = document.body.querySelector('inventory-management').shadowRoot;
		shell.querySelector('rule-update').shadowRoot.getElementById('updateRuleModal').close();
		shell.getElementById('ruleList').shadowRoot.getElementById('ruleGrid').clearCache();
	}

	_ruleUpdateError(event) {
		var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
		toast.text = event.detail.request.xhr.statusText;
		toast.open();
	}
}

window.customElements.define('rule-update', ruleUpdate);
