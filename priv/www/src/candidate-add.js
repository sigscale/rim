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

class candidateAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="addCandidateModal" modal>
				<app-toolbar>
					<div main-title>Add Candidate</div>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
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
							dialog-dismiss>
						Cancel
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax
					id="candidateAddAjax"
					content-type="application/json"
					loading="{{loading}}"
					on-response="_candidateAddResponse"
					on-error="_candidateAddError">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				value: false
			},
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
		var shell = document.body.querySelector('inventory-management').shadowRoot;
		shell.querySelector('candidate-add').shadowRoot.getElementById('addCandidateModal').close();
		shell.getElementById('candidateList').shadowRoot.getElementById('candidateGrid').clearCache();
	}

	_candidateAddError(event) {
		var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
		toast.text = event.detail.request.xhr.statusText;
		toast.open();
	}
}

window.customElements.define('candidate-add', candidateAdd);
