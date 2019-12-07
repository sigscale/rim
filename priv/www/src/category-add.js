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

class categoryAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
		<paper-dialog class="dialog" id="addCategoryModal" modal>
			<paper-toolbar>
				<div slot="top"><h2>Add Category</h2></div>
			</paper-toolbar>
				<paper-input
						id="categoryName"
						label="Name"
						value="{{category.categoryName}}">
				</paper-input>
				<paper-input
						id="categoryDesc"
						label="Description"
						value="{{category.categoryDesc}}">
				</paper-input>
				<paper-input
						id="categoryVersion"
						label="Version"
						value="{{category.categoryVersion}}">
				</paper-input>
				<paper-input
						id="categoryClass"
						label="Class"
						value="{{category.categoryClass}}">
				</paper-input>
				<paper-input
						id="categoryStatus"
						label="Status"
						value="{{category.categoryStatus}}">
				</paper-input>
				<paper-input
						id="categoryParent"
						label="Parent"
						value="{{category.categoryParent}}">
				</paper-input>
				<paper-input
						id="categoryRoot"
						label="Root"
						value="{{category.categoryRoot}}">
				</paper-input>
				<div class="buttons">
					<paper-button
							raised
							class="submit-button"
							on-tap="_addCategory">
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
			id="categoryAddAjax"
			content-type="application/json"
			on-loading-changed="_onLoadingChanged"
			on-response="_categoryAddResponse"
			on-error="_categoryAddError">
		</iron-ajax>
		`;
	}

	static get properties() {
		return {
			category: {
				type: Object,
			}
		}
	}

   ready() {
      super.ready()
	}

	_addCategory() {
		var ajax = this.$.categoryAddAjax;
		ajax.method = "POST";
		ajax.url = "/resourceCatalogManagement/v3/resourceCategory/";
		var cat = new Object();
		if(this.$.categoryName.value) {
			cat.name = this.$.categoryName.value;
		}
		if(this.$.categoryDesc.value) {
			cat.description = this.$.categoryDesc.value;
		}
		if(this.$.categoryVersion.value) {
			cat.version = this.$.categoryVersion.value;
		}
		if(this.$.categoryClass.value) {
			cat.class = this.$.categoryClass.value;
		}
		if(this.$.categoryStatus.value) {
			cat.status = this.$.categoryStatus.value;
		}
		if(this.$.categoryParent.value) {
			cat.parent = this.$.categoryParent.value;
		}
		if(this.$.categoryRoot.value) {
			cat.root = this.$.categoryRoot.value;
		}
		ajax.body = JSON.stringify(cat);
		ajax.generateRequest();
	}

	_categoryAddResponse() {
		document.body.querySelector('inventory-management').shadowRoot.querySelector('category-add').shadowRoot.getElementById('addCategoryModal').close();
		document.getElementById("categoryGrid").clearCache();
	}

	_categoryAddError(event) {
		var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
		toast.text = event.detail.request.xhr.statusText;
		toast.open();
	}
}

window.customElements.define('category-add', categoryAdd);
