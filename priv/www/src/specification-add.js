/*
 * @license
 * Copyright 2018 - 2024 SigScale Global Inc.
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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

class specificationAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="specificationAddModal" modal>
				<app-toolbar>
					<div main-title>Add Specification</div>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<paper-input
						label="Name"
						value="{{specificationMame}}">
				</paper-input>
				<paper-textarea
						label="Description"
						value="{{specificationDescription}}">
				</paper-textarea>
				<paper-input
						label="Version"
						value="{{specificationVersion}}">
				</paper-input>
				<paper-input
						label="Class"
						value="{{specificationType}}">
				</paper-input>
				<paper-input
						label="Status"
						value="{{specificationStatus}}">
				</paper-input>
				<paper-input
						label="Category"
						value="{{specificationCategory}}">
				</paper-input>
				<paper-input
						label="Bundle"
						value="{{specificationBundle}}">
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
					id="specificationAddAjax"
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
			specificationName: {
				type: String
			},
			specificationDescription: {
				type: String
			},
			specificationType: {
				type: String
			},
			specificationStatus: {
				type: String
			},
			specificationVersion: {
				type: String
			},
			specificationCategory: {
				type: String
			},
			specificationBundle: {
				type: Boolean
			}
		}
	}

	ready() {
		super.ready()
	}

	_cancel() {
		this.$.specificationAddModal.close();
		this.specificationName = null;
		this.specificationDescription = null;
		this.specificationType = null;
		this.specificationStatus = null;
		this.specificationVersion = null;
		this.specificationCategory = null;
		this.specificationBundle = false;
	}

	_add() {
		var ajax = this.$.specificationAddAjax;
		ajax.method = "POST";
		ajax.url = "/resourceCatalogManagement/v4/resourceSpecification/";
		var spec = new Object();
		if(this.specificationName) {
			spec.name = this.specificationName;
		}
		if(this.specificationDescription) {
			spec.description = this.specificationDescription;
		}
		if(this.specificationType) {
			spec['@type'] = this.specificationType;
		}
		if(this.specificationStatus) {
			spec.lifecycleStatus = this.specificationStatus;
		}
		if(this.specificationVersion) {
			spec.version = this.specificationVersion;
		}
		if(this.specificationCategory) {
			spec.category = this.specificationCategory;
		}
		if(this.specificationBundle) {
			spec.isBundle = this.specificationBundle;
		}
		ajax.body = JSON.stringify(spec);
		ajax.generateRequest();
	}

	_response() {
		this.$.specificationAddModal.close();
		this.specificationName = null;
		this.specificationDescription = null;
		this.specificationType = null;
		this.specificationStatus = null;
		this.specificationVersion = null;
		this.specificationCategory = null;
		this.specificationBundle = false;
		document.body.querySelector('inventory-management').shadowRoot.getElementById('specificationList').shadowRoot.getElementById('specificationGrid').clearCache();
	}

	_error(event) {
		var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
		toast.text = event.detail.request.xhr.statusText;
		toast.open();
	}
}

window.customElements.define('specification-add', specificationAdd);
