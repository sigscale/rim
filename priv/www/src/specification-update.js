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
import '@polymer/paper-dropdown-menu/paper-dropdown-menu.js';
import '@polymer/paper-listbox/paper-listbox.js';
import '@polymer/paper-item/paper-item.js'
import '@polymer/paper-checkbox/paper-checkbox.js'
import '@polymer/iron-collapse/iron-collapse.js';
import './style-element.js';

class specificationUpdate extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
		<paper-dialog class="dialog" id="specificationUpdateModal" modal>
			<app-toolbar>
				<div main-title>Update Specification</div>
			</app-toolbar>
			<paper-progress
					indeterminate
					class="slow red"
					disabled="{{!loading}}">
			</paper-progress>
				<paper-input
						label="Id"
						value="{{specificationId}}"
						disabled>
				</paper-input>
				<paper-input
						label="Name"
						value="{{specificationName}}"
						required>
				</paper-input>
				<paper-textarea
						label="Description"
						value="{{specificationDescription}}">
				</paper-textarea>
				<paper-input
						label="Class"
						value="{{specificationType}}">
				</paper-input>
				<paper-dropdown-menu
					class="drop"
					label="Status"
					value="{{specificationStatus}}"
					no-animations="true">
					<paper-listbox
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
				<paper-input
					label="Version"
					value="{{specificationVersion}}">
				</paper-input>
				<paper-checkbox
					value="{{specificationBundle}}">
					Is Bundle
				</paper-checkbox>
				<paper-checkbox
					value="{{specificationEnabled}}">
					Enabled
				</paper-checkbox>
				<div>
					<span>Feature</span>
						<paper-icon-button id="featSpecCollapseButton"
							id="collapseFeat"
							icon="arrow-drop-down"
							on-click="_collapseFeat">
						</paper-icon-button>
				</div>
				<iron-collapse
						id="featSpecCollapse"
						opened="{{featSpecOpened}}">
					<template is="dom-repeat" items="[[specificationFeature]]">
						<div>
						<hr>
						<paper-input
							label="Id"
							value="{{item.id}}">
						</paper-input>
						<paper-input
							label="Name"
							value="{{item.name}}">
						</paper-input>
						<paper-textarea
								label="Description"
								value="{{item.description}}">
						</paper-textarea>
						</div>
					</template>
				</iron-collapse>
				<div>
					<span>Characteristics</span>
						<paper-icon-button id="charSpecCollapseButton"
							icon="arrow-drop-down"
							on-click="_collapseChars">
						</paper-icon-button>
				</div>
				<iron-collapse
						id="charSpecCollapse"
						opened="{{charSpecOpened}}">
					<template is="dom-repeat" items="[[specificationChars]]">
						<div>
						<hr>
						<paper-input
							label="Name"
							value="{{item.name}}">
						</paper-input>
						<paper-textarea
							label="Description"
							value="{{item.description}}">
						</paper-textarea>
						<paper-input
							label="ValueType"
							value="{{item.valueType}}">
						</paper-input>
						</div>
					</template>
				</iron-collapse>
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
			id="specificationDeleteAjax"
			loading="{{loading}}"
			on-response="_response"
			on-error="_error">
		</iron-ajax>
		<iron-ajax
			id="specificationUpdateAjax"
			content-type="application/merge-patch+json"
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
			featSpecOpened: {
				type: Boolean,
				observer: '_resize'
			},
			charSpecOpened: {
				type: Boolean,
				observer: '_resize'
			},
			specificationId: {
				type: String
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
			specificationBundle: {
				type: Boolean
			},
			specificationEnabled: {
				type: Boolean
			},
			specificationFeatures: {
				type: Array
			},
			specificationChars: {
				type: Array
			}
		}
	}

	ready() {
		super.ready()
	}

	_activeItemChanged(item) {
		if(item) {
			this.specificationId = item.id;
			this.specificationName = item.name;
			this.specificationDescription = item.description;
			this.specificationType = item.type;
			this.specificationStatus = item.status;
			this.specificationVersion = item.version;
			this.specificationBundle = item.bundle;
			this.specificationEnabled = item.enabled;
			this.specificationFeatures = item.features;
			this.specificationChars = item.chars;
			if(this.featSpecOpened == true) {
				this.$.featSpecCollapse.hide();
			}
			if(this.charSpecOpened == true) {
				this.$.charSpecCollapse.hide();
			}
		} else {
			this.specificationId = null;
			this.specificationName = null;
			this.specificationDescription = null;
			this.specificationVersion = null;
			this.specificationBundle = false;
			this.specificationEnabled = false;
			this.specificationFeatures = [];
			this.specificationChars = [];
		}
	}

	_cancel() {
		this.$.specificationUpdateModal.close();
		this.specificationId = null;
		this.specificationName = null;
		this.specificationDescription = null;
		this.specificationVersion = null;
		this.specificationBundle = false;
		this.specificationEnabled = false;
		this.specificationFeatures = [];
		this.specificationChars = [];
	}

	_delete() {
		var ajax = this.$.specificationDeleteAjax;
		ajax.method = "DELETE";
		ajax.url = "/resourceCatalogManagement/v4/resourceSpecification/" + this.$.addSpecId.value;
		ajax.generateRequest();
		this.$.specificationUpdateModal.close();
	}

	_update() {
		var ajax = this.$.specificationUpdateAjax;
		ajax.method = "PATCH";
		ajax.url = "/resourceCatalogManagement/v4/resourceSpecification/" + this.$.addSpecId.value;
		var spec = new Object();
		if(this.specificationId) {
			specId = this.specificationId;
		}
		if(this.specificationName) {
			spec.name = this.specificationName;
		}
		if(this.specificationDescription) {
			spec.description = this.specificationDescription;
		}
		if(this.specificationType) {
			spec["@type"] = this.specificationType;
		}
		if(this.specificationStatus) {
			spec.lifecycleStatus = this.specificationStatus;
		}
		ajax.body = JSON.stringify(spec);
		ajax.generateRequest();
	}

	_response() {
		this.$.specificationUpdateModal.close();
		document.body.querySelector('inventory-management').shadowRoot.getElementById('specificationList').shadowRoot.getElementById('specificationGrid').clearCache();
	}

	_error(event) {
		var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
		toast.text = event.detail.request.xhr.statusText;
		toast.open();
	}

	_collapseFeat(event) {
		if(this.featSpecOpened == false) {
			this.$.featSpecCollapse.show();
			this.$.featSpecCollapseButton.icon = "arrow-drop-up";
		} else {
			this.$.featSpecCollapse.hide();
			this.$.featSpecCollapseButton.icon = "arrow-drop-down";
		}
	}

	_collapseChars(event) {
		if(this.charSpecOpened == false) {
			this.$.charSpecCollapse.show();
			this.$.charSpecCollapseButton.icon = "arrow-drop-up";
		} else {
			this.$.charSpecCollapse.hide();
			this.$.charSpecCollapseButton.icon = "arrow-drop-down";
		}
	}

	_resize() {
		this.$.specificationUpdateModal.notifyResize();
	}
}

window.customElements.define('specification-update', specificationUpdate);
