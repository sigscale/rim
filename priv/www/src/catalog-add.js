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

class catalogAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
		<paper-dialog class="dialog" id="addCatalogModal" modal>
			<paper-toolbar>
				<div slot="top"><h2>Add Catalog</h2></div>
			</paper-toolbar>
				<paper-input
						id="catalogName"
						label="Name"
						value="{{catalog.catalogName}}">
				</paper-input>
				<paper-input
						id="catalogDesc"
						label="Description"
						value="{{catalog.catalogDesc}}">
				</paper-input>
				<paper-input
						id="catalogVersion"
						label="Version"
						value="{{catalog.catalogVersion}}">
				</paper-input>
				<div class="buttons">
					<paper-button
							raised
							class="submit-button"
							on-tap="_addCatalog">
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
			id="catalogAddAjax"
			content-type="application/json"
			on-loading-changed="_onLoadingChanged"
			on-response="_catalogAddResponse"
			on-error="_catalogAddError">
		</iron-ajax>
		`;
	}

	static get properties() {
		return {
			catalog: {
				type: Object,
			}
		}
	}

   ready() {
      super.ready()
	}

	_addCatalog() {
		var ajax = this.$.catalogAddAjax;
		ajax.method = "POST";
		ajax.url = "/resourceCatalogManagement/v3/resourceCatalog/";
		var cat = new Object();
		if(this.$.catalogName.value) {
			cat.name = this.$.catalogName.value;
		}
		if(this.$.catalogDesc.value) {
			cat.description = this.$.catalogDesc.value;
		}
		if(this.$.catalogVersion.value) {
			cat.version = this.$.catalogVersion.value;
		}
		ajax.body = JSON.stringify(cat);
		ajax.generateRequest();
	}

	_catalogAddResponse() {
		document.body.querySelector('inventory-management').shadowRoot.querySelector('catalog-add').shadowRoot.getElementById('addCatalogModal').close();
	}

	_catalogAddError(event) {
		var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
		toast.text = event.detail.request.xhr.statusText;
		toast.open();
	}
}

window.customElements.define('catalog-add', catalogAdd);
