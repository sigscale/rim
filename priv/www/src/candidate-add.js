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

class candidateAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="candidateAddModal" modal>
				<app-toolbar>
					<div main-title>Add Candidate</div>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<paper-input
						label="Name"
						value="{{candidateName}}">
				</paper-input>
				<paper-textarea
						label="Description"
						value="{{candidateDescription}}">
				</paper-textarea>
				<paper-input
						label="Version"
						value="{{candidateVersion}}">
				</paper-input>
				<paper-input
						label="Class"
						value="{{candidateType}}">
				</paper-input>
				<paper-input
						label="Status"
						value="{{candidateStatus}}">
				</paper-input>
				<div class="buttons">
					<paper-button
							raised
							class="submit-button"
							on-tap="_add">
						Add
					</paper-button>
					<paper-button
							class="cancel-button"
							on-tap="_cancel">
						Cancel
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax
					id="candidateAddAjax"
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
			candidateName: {
				type: String
			},
			candidateDescription: {
				type: String
			},
			candidateType: {
				type: String
			},
			candidateVersion: {
				type: String
			},
			candidateStatus: {
				type: String
			}
		}
	}

	ready() {
		super.ready()
	}

	_cancel() {
		this.$.candidateAddModal.close();
		this.candidateName = null;
		this.candidateDescription = null;
		this.candidateType = null;
		this.candidateVersion = null;
		this.candidateStatus = null;
	}

	_add() {
		var ajax = this.$.candidateAddAjax;
		ajax.method = "POST";
		ajax.url = "/resourceCatalogManagement/v4/resourceCandidate/";
		var cand = new Object();
		if(this.candidateName) {
			cand.name = this.candidateName;
		}
		if(this.candidateDescription) {
			cand.description= this.candidateDescription;
		}
		if(this.candidateVersion) {
			cand.version = this.candidateVersion;
		}
		if(this.candidateType) {
			cand['@type'] = this.candidateType;
		}
		if(this.candidateStatus) {
			cand.lifecycleStatus = this.candidateStatus;
		}
		ajax.body = JSON.stringify(cand);
		ajax.generateRequest();
	}

	_response() {
		this.$.candidateAddModal.close();
		this.candidateName = null;
		this.candidateDescription = null;
		this.candidateType = null;
		this.candidateVersion = null;
		this.candidateStatus = null;
		document.body.querySelector('inventory-management').shadowRoot.getElementById('candidateList').shadowRoot.getElementById('candidateGrid').clearCache();
	}

	_error(event) {
		var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
		toast.text = event.detail.request.xhr.statusText;
		toast.open();
	}
}

window.customElements.define('candidate-add', candidateAdd);
