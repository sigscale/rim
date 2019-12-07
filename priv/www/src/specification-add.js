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
import '@polymer/app-layout/app-toolbar/app-toolbar.js';
import '@polymer/paper-progress/paper-progress.js';
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-button/paper-button.js';
import '@polymer/paper-dropdown-menu/paper-dropdown-menu.js';
import '@polymer/paper-listbox/paper-listbox.js';
import '@polymer/paper-item/paper-item.js'
import '@polymer/paper-checkbox/paper-checkbox.js'
import '@polymer/iron-collapse/iron-collapse.js';
import './style-element.js';

class specificationAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
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
					value="{{specification.specificationName}}">
			</paper-input>
			<paper-input
					id="specificationDesc"
					label="Description"
					value="{{specification.specificationDesc}}">
			</paper-input>
			<paper-input
					id="specificationVersion"
					label="Version"
					value="{{specification.specificationVersion}}">
			</paper-input>
			<paper-input
					id="specificationClass"
					label="Class"
					value="{{specification.specificationClass}}">
			</paper-input>
			<paper-input
					id="specificationStatus"
					label="Status"
					value="{{specification.specificationStatus}}">
			</paper-input>
			<paper-input
					id="specificationCategory"
					label="Category"
					value="{{specification.specificationCategory}}">
			</paper-input>
			<paper-input
					id="specificationBundle"
					label="Bundle"
					value="{{specification.specificationBundle}}">
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
						dialog-dismiss>
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
			}
		}
	}

	ready() {
		super.ready()
	}

	_addSpecification() {
		var ajax = this.$.specificationAddAjax;
		ajax.method = "POST";
		ajax.url = "/resourceCatalogManagement/v3/resourceSpecification/";
		var spec = new Object();
		if(this.$.specificationName.value) {
			spec.name = this.$.specificationName.value;
		}
		if(this.$.specificationDesc.value) {
			spec.description = this.$.specificationDesc.value;
		}
		if(this.$.specificationVersion.value) {
			spec.version = this.$.specificationVersion.value;
		}
		if(this.$.specificationClass.value) {
			spec.class = this.$.specificationClass.value;
		}
		if(this.$.specificationStatus.value) {
			spec.status = this.$.specificationStatus.value;
		}
		ajax.body = JSON.stringify(spec);
		ajax.generateRequest();
	}

	_specificationAddResponse() {
		var shell = document.body.querySelector('inventory-management').shadowRoot;
		shell.querySelector('specification-add').shadowRoot.getElementById('addSpecificationModal').close();
		shell.getElementById('specificationList').shadowRoot.getElementById('specificationGrid').clearCache();
	}

	_specificationAddError(event) {
		var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
		toast.text = event.detail.request.xhr.statusText;
		toast.open();
	}
}

window.customElements.define('specification-add', specificationAdd);
