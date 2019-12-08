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

class specificationAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="addSpecificationModal" modal>
				<app-toolbar>
					<div main-title>Add Specification</div>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<paper-input
						id="specificationName"
						label="Name"
						value="{{specification.name}}">
				</paper-input>
				<paper-input
						id="specificationDescription"
						label="Description"
						value="{{specification.desription}}">
				</paper-input>
				<paper-input
						id="specificationVersion"
						label="Version"
						value="{{specification.version}}">
				</paper-input>
				<paper-input
						id="specificationClass"
						label="Class"
						value="{{specification.class}}">
				</paper-input>
				<paper-input
						id="specificationStatus"
						label="Status"
						value="{{specification.status}}">
				</paper-input>
				<paper-input
						id="specificationCategory"
						label="Category"
						value="{{specification.category}}">
				</paper-input>
				<paper-input
						id="specificationBundle"
						label="Bundle"
						value="{{specification.bundle}}">
				</paper-input>
				<div class="buttons">
					<paper-button
							raised
							class="submit-button"
							on-tap="_addSpecification">
						Add
					</paper-button>
					<paper-button
							class="cancel-button"
							on-tap="_cancelSpecification>
						Cancel
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax
					id="specificationAddAjax"
					content-type="application/json"
					loading="{{loading}}"
					on-response="_specificationAddResponse"
					on-error="_specificationAddError">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				value: false
			},
			specification: {
				type: Object,
				value: function() {
					return {};
				}
			}
		}
	}

	ready() {
		super.ready()
	}

	_cancelSpecification() {
		this.$.addSpecificationModal.close();
		this.specification = {};
	}

	_addSpecification() {
		var ajax = this.$.specificationAddAjax;
		ajax.method = "POST";
		ajax.url = "/resourceCatalogManagement/v3/resourceSpecification/";
		var spec = new Object();
		if(this.specification.name) {
			spec.name = this.specification.name;
		}
		if(this.specification.description) {
			spec.description = this.specification.description;
		}
		if(this.specification.version) {
			spec.version = this.specification.version;
		}
		if(this.specification.class) {
			spec.baseType = this.specification.class;
		}
		if(this.specification.status) {
			spec.lifecycleStatus = this.specification.status;
		}
		ajax.body = JSON.stringify(spec);
		ajax.generateRequest();
	}

	_specificationAddResponse() {
		this.$.addSpecificationModal.close();
		this.specification = {};
		document.body.querySelector('inventory-management').shadowRoot.getElementById('specificationList').shadowRoot.getElementById('specificationGrid').clearCache();
	}

	_specificationAddError(event) {
		var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
		toast.text = event.detail.request.xhr.statusText;
		toast.open();
	}
}

window.customElements.define('specification-add', specificationAdd);
