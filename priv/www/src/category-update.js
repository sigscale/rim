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

class categoryUpdateList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
		<paper-dialog class="dialog" id="updateCategoryModal" modal>
			<paper-toolbar>
				<div slot="top"><h2>Update Catalog</h2></div>
			</paper-toolbar>
				<paper-input
						id="categorySpecId"
						label="Id"
						value="{{category.categoryId}}"
						disabled>
				</paper-input>
				<paper-input
						id="categorySpecName"
						label="Name"
						value="{{category.categoryName}}"
						required>
				</paper-input>
				<paper-input
						id="categorySpecDesc"
						label="Description"
						value="{{category.categoryDescription}}">
				</paper-input>
				<paper-input
						id="categorySpecType"
						label="Class"
						value="{{category.categoryClass}}">
				</paper-input>
				<paper-dropdown-menu
					id="categoryStatus" 
					class="drop"
					label="Status"
					value="{{category.categoryStatus}}"
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
				<paper-input
						id="categorySpecParent"
						label="Parent"
						value="{{category.categoryParent}}">
				</paper-input>
				<paper-input
						id="categorySpecRoot"
						label="Root"
						value="{{category.categoryRoot}}">
				</paper-input>
				<div class="buttons">
					<paper-button
							raised
							class="update-button"
							on-tap="_updateSpec">
						Update
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
			id="deleteCategoryAjax"
			on_response="_deleteCategoryResponse"
			on-error="_deleteCategoryError">
		</iron-ajax>
		<iron-ajax
			id="categoryUpdateAjax"
			content-type="application/merge-patch+json"
			on-loading-changed="_onLoadingChanged"
			on-response="_categorySpecResponse"
			on-error="_categorySpecError">
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

	_deleteSpec() {
		var ajax1 = this.$.deleteCategoryAjax;
		ajax1.method = "DELETE";
		ajax1.url = "/resourceCatalogManagement/v3/resourceCategory/" + this.$.categorySpecId.value;
		ajax1.generateRequest();
		var deleteObj =  document.body.querySelector('inventory-management').shadowRoot.querySelector('category-update').shadowRoot.getElementById('updateCategoryModal');
		deleteObj.close();
	}

	_updateSpec() {
		var ajax = this.$.categoryUpdateAjax;
		ajax.method = "PATCH";
		ajax.url = "/resourceCatalogManagement/v3/resourceCategory/" + this.$.categorySpecId.value;
		var cat = new Object();
		if(this.$.categorySpecName.value) {
			cat.name = this.$.categorySpecName.value;
		}
		if(this.$.categorySpecDesc.value) {
			cat.description = this.$.categorySpecDesc.value;
		}
		if(this.$.categorySpecType) {
			cat["@type"] = this.$.categorySpecType.value;
		}
		if(this.$.categoryStatus.value) {
			cat.lifecycleStatus = this.$.categoryStatus.value;
		}
		if(this.$.categorySpecParent.value) {
			cat.parentId = this.$.categorySpecParent.value;
		}
		if(this.$.categorySpecRoot.value) {
			cat.isRoot = this.$.categorySpecRoot.value;
		}
		ajax.body = JSON.stringify(cat);
		ajax.generateRequest();
	}

	_categorySpecResponse() {
		document.getElementById("categoryGrid").clearCache();
	}
}

window.customElements.define('category-update', categoryUpdateList);
