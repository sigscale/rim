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
import '@polymer/paper-fab/paper-fab.js';
import '@polymer/iron-icons/iron-icons.js';
import '@polymer/paper-dialog/paper-dialog.js';
import '@polymer/paper-toolbar/paper-toolbar.js';
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-button/paper-button.js';
import '@polymer/paper-dropdown-menu/paper-dropdown-menu.js';
import '@polymer/paper-listbox/paper-listbox.js';
import '@polymer/paper-item/paper-item.js'
import '@polymer/paper-checkbox/paper-checkbox.js'
import '@polymer/iron-collapse/iron-collapse.js';
import './style-element.js';

class ruleUpdateList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
		<paper-dialog class="dialog" id="updateRuleModal" modal>
			<paper-toolbar>
				<div slot="top"><h2>Update Rule</h2></div>
			</paper-toolbar>
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
							on-tap="_updateSpec">
						Update
					</paper-button>
					<paper-button
							class="cancel-button"
							dialog-dismiss
							on-tap="cancelSpec">
						Cancel
					</paper-button>
				</div>
		</paper-dialog>
		<iron-ajax
			id="ruleUpdateAjax"
			content-type="application/json-patch+json"
			on-loading-changed="_onLoadingChanged"
			on-response="_addSpecResponse"
			on-error="_addSpecError">
		</iron-ajax>
		<iron-ajax
			id="ruleDeleteAjax"
			content-type="application/json"
			on-loading-changed="_onLoadingChanged"
			on-response="_addDelResponse"
			on-error="_addDelError">
		</iron-ajax>
		`;
	}

	static get properties() {
		return {
			rule: {
				type: Object,
			}
		}
	}

	ready() {
		super.ready()
	}

	_updateSpec() {
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
}

window.customElements.define('rules-update', ruleUpdateList);
