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

class candUpdateList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
		<paper-dialog class="dialog" id="updateCandModal" modal>
			<paper-toolbar>
				<div slot="top"><h2>Update Specification</h2></div>
			</paper-toolbar>
				<paper-input
						id="addCandId"
						label="Name"
						value="{{candidate.candId}}"
						disabled>
				</paper-input>
				<paper-input
						id="addCandName"
						label="Name"
						value="{{candidate.candName}}"
						required>
				</paper-input>
				<paper-input
						id="addCandDesc"
						label="Description"
						value="{{candidate.candDesc}}">
				</paper-input>
				<paper-input
						id="addCandType"
						label="Class"
						value="{{candidate.candClass}}">
				</paper-input>
				<paper-dropdown-menu
					id="candStatus"
					class="drop"
					label="Status"
					value="{{candidate.candStatus}}"
					no-animations="true">
					<paper-listbox
							id="updateStatus"
							slot="dropdown-content">
						<paper-item>In Study</paper-item>
						<paper-item>In Design</paper-item>
						<paper-item>In Test</paper-item>
						<paper-item>Rejected</paper-item>
						<paper-item>Active</paper-item>
						<paper-item>Launched</paper-item>
						<paper-item>Retired</paper-item>
						<paper-item>Obsolete</paper-item>
					</paper-listbox>
				</paper-dropdown-menu>
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
					<paper-button
							toggles
							raised
							class="delete-button"
							on-tap="_deleteSpec">
						Delete
					</paper-button>
				</div>
		</paper-dialog>
		<iron-ajax
			id="candUpdateAjax"
			content-type="application/merge-patch+json"
         on-loading-changed="_onLoadingChanged"
         on-response="_addSpecResponse"
         on-error="_addSpecError">
		</iron-ajax>
		`;
	}

	static get properties() {
		return {
			candidate: {
				type: Object,
			}
		}
	}

   ready() {
      super.ready()
	}

	_updateSpec() {
		var ajax = this.$.candUpdateAjax;
		ajax.method = "PATCH";
		ajax.url = "/resourceCatalogManagement/v3/resourceCandidate/" + this.$.addCandId.value;
		var cand = new Object();
		if(this.$.addCandName.value) {
         cand.name = this.$.addCandName.value;
      }
		if(this.$.addCandDesc.value) {
         cand.description = this.$.addCandDesc.value;
      }
		if(this.$.addCandType.value) {
         cand["@type"] = this.$.addCandType.value;
      }
		if(this.$.candStatus.value) {
         cand.lifecycleStatus = this.$.candStatus.value;
      }
		ajax.body = JSON.stringify(cand);
		ajax.generateRequest();
	}
}

window.customElements.define('candidate-update', candUpdateList);
