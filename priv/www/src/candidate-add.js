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

class candidateAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
		<paper-dialog class="dialog" id="addCandidateModal" modal>
			<paper-toolbar>
				<div slot="top"><h2>Add Category</h2></div>
			</paper-toolbar>
				<paper-input
						id="candidateName"
						label="Name"
						value="{{candidate.candidateName}}">
				</paper-input>
				<paper-input
						id="candidateDesc"
						label="Description"
						value="{{candidate.candidateDesc}}">
				</paper-input>
				<paper-input
						id="candidateVersion"
						label="Version"
						value="{{candidate.candidateVersion}}">
				</paper-input>
				<paper-input
						id="candidateClass"
						label="Class"
						value="{{candidate.candidateClass}}">
				</paper-input>
				<paper-input
						id="candidateStatus"
						label="Status"
						value="{{candidate.candidateStatus}}">
				</paper-input>
				<div class="buttons">
					<paper-button
							raised
							class="submit-button"
							on-tap="_addCandidate">
						Add
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
			id="candidateAddAjax"
			content-type="application/json"
			on-loading-changed="_onLoadingChanged"
			on-response="_candidateAddResponse"
			on-error="_candidateAddError">
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

	_addCandidate() {
		var ajax = this.$.candidateAddAjax;
		ajax.method = "POST";
		ajax.url = "/resourceCatalogManagement/v3/resourceCandidate/";
		var can = new Object();
		if(this.$.candidateName.value) {
			can.name = this.$.candidateName.value;
		}
		if(this.$.candidateDesc.value) {
			can.description = this.$.candidateDesc.value;
		}
		if(this.$.candidateVersion.value) {
			can.version = this.$.candidateVersion.value;
		}
		if(this.$.candidateClass.value) {
			can.class = this.$.candidateClass.value;
		}
		if(this.$.candidateStatus.value) {
			can.status = this.$.candidateStatus.value;
		}
		ajax.body = JSON.stringify(can);
		ajax.generateRequest();
	}

	_candidateAddResponse() {
		document.body.querySelector('inventory-management').shadowRoot.querySelector('candidate-add').shadowRoot.getElementById('addCandidateModal').close();
	}
}

window.customElements.define('candidate-add', candidateAdd);
