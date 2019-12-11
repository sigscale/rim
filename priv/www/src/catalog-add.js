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

class catalogAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="catalogAddModal" modal>
				<app-toolbar>
					<div main-title>Add Catalog</div>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<paper-input
						label="Name"
						value="{{catalogName}}">
				</paper-input>
				<paper-input
						label="Description"
						value="{{catalogDescription}}">
				</paper-input>
				<paper-input
						label="Version"
						value="{{catalogVersion}}">
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
					id="catalogAddAjax"
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
			catalogName: {
				type: String
			},
			catalogDescription: {
				type: String
			},
			catalogVersion: {
				type: String
			}
		}
	}

   ready() {
      super.ready()
	}

	_cancel() {
		this.$.catalogAddModal.close();
		this.catalogName = null;
		this.catalogDescription = null;
		this.catalogVersion = null;
	}

	_add() {
		var ajax = this.$.catalogAddAjax;
		ajax.method = "POST";
		ajax.url = "/resourceCatalogManagement/v3/resourceCatalog/";
		var cat = new Object();
		if(this.catalogName) {
			cat.name = this.catalogName;
		}
		if(this.catalogDescription) {
			cat.description = this.catalogDescription;
		}
		if(this.catalogType) {
			cat['@type'] = this.catalogType;
		}
		if(this.catalogStatus) {
			cat.lifecycleStatus = this.catalogStatus;
		}
		if(this.catalogVersion) {
			cat.version = this.catalogVersion;
		}
		ajax.body = JSON.stringify(cat);
		ajax.generateRequest();
	}

	_response() {
		this.$.catalogAddModal.close();
		this.catalogName = null;
		this.catalogDescription = null;
		this.catalogVersion = null;
		document.body.querySelector('inventory-management').shadowRoot.getElementById('catalogList').shadowRoot.getElementById('catalogGrid').clearCache();
	}

	_error(event) {
		var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
		toast.text = event.detail.request.xhr.statusText;
		toast.open();
	}
}

window.customElements.define('catalog-add', catalogAdd);
