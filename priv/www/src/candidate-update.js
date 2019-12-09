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
import '@polymer/iron-icons/iron-icons.js';
import '@polymer/paper-dialog/paper-dialog.js';
import '@polymer/app-layout/app-toolbar/app-toolbar.js';
import '@polymer/paper-progress/paper-progress.js';
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-button/paper-button.js';
import '@polymer/paper-dropdown-menu/paper-dropdown-menu.js';
import '@polymer/paper-listbox/paper-listbox.js';
import '@polymer/paper-item/paper-item.js'
import './style-element.js';

class candidateUpdate extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="candidateUpdateModal" modal>
				<app-toolbar>
					<div main-title>Update Candidate</div>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
					<paper-input
							id="addCandId"
							label="Id"
							value="{{candidateId}}"
							disabled>
					</paper-input>
					<paper-input
							id="addCandName"
							label="Name"
							value="{{candidateName}}"
							required>
					</paper-input>
					<paper-input
							id="addCandDesc"
							label="Description"
							value="{{candidateDescription}}">
					</paper-input>
					<paper-input
							id="addCandType"
							label="Class"
							value="{{candidateType}}">
					</paper-input>
					<paper-dropdown-menu
						id="candStatus"
						class="drop"
						label="Status"
						value="{{candidateStatus}}"
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
								on-tap="_update">
							Update
						</paper-button>
						<paper-button
								class="cancel-button"
								on-tap="_cancel">
							Cancel
						</paper-button>
						<paper-button
								toggles
								raised
								class="delete-button"
								on-tap="_delete">
							Delete
						</paper-button>
					</div>
			</paper-dialog>
			<iron-ajax
				id="deleteCandidateAjax"
				loading="{{loading}}"
				on-response="_candidateUpdateResponse"
				on-error="_candidateUpdateError">
			</iron-ajax>
			<iron-ajax
				id="candUpdateAjax"
				content-type="application/merge-patch+json"
				loading="{{loading}}"
				on-response="_candidateUpdateResponse"
				on-error="_candidateUpdateError">
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
			candidateId: {
				type: String
			},
			candidateName: {
				type: String
			},
			candidateDescription: {
				type: String
			},
			candidateType: {
				type: String
			}
		}
	}

	ready() {
		super.ready()
	}

	_activeItemChanged(item) {
		if(item) {
			this.candidateId = item.id;
			this.candidateName = item.name;
			this.candidateDescription = item.description;
			this.candidateType = item.type;
			this.$.candidateUpdateModal.open();
		} else {
			this.candidateId = null;
			this.candidateName = null;
			this.candidateDescription = null;;
			this.candidateType = null;
		}
	}

	_cancel() {
		this.$.candidateUpdateModal.close();
		this.candidateId = null;
		this.candidateName = null;
		this.candidateDescription = null;;
		this.candidateType = null;
	}

	_delete() {
		var ajax = this.$.deleteCandidateAjax;
		ajax.method = "DELETE";
		ajax.url = "/resourceCatalogManagement/v3/resourceCandidate/" + this.$.addCandId.value;
		ajax.generateRequest();
	}

	_update() {
		var ajax = this.$.candUpdateAjax;
		ajax.method = "PATCH";
		ajax.url = "/resourceCatalogManagement/v3/resourceCandidate/" + this.$.addCandId.value;
		var cand = new Object();
		if(this.candidateId) {
			cand.id = this.candidateId;
		}
		if(this.candidateName) {
			cand.name = this.candidateName;
		}
		if(this.candidateDescription) {
			cand.description = this.candidateDescription;
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

	_candidateUpdateResponse() {
		this.$.candidateUpdateModal.close();
		document.body.querySelector('inventory-management').shadowRoot.getElementById('candidateList').shadowRoot.getElementById('candidateGrid').clearCache();
	}

	_candidateUpdateError(event) {
		var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
		toast.text = event.detail.request.xhr.statusText;
		toast.open();
	}
}

window.customElements.define('candidate-update', candidateUpdate);
