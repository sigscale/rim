/*
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

class categoryAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="addCategoryModal" modal>
				<app-toolbar>
					<div main-title>Add Category</div>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<paper-input
						id="categoryName"
						label="Name"
						value="{{category.name}}">
				</paper-input>
				<paper-input
						id="categoryDesc"
						label="Description"
						value="{{category.description}}">
				</paper-input>
				<paper-input
						id="categoryVersion"
						label="Version"
						value="{{category.version}}">
				</paper-input>
				<paper-input
						id="categoryClass"
						label="Class"
						value="{{category.class}}">
				</paper-input>
				<paper-input
						id="categoryStatus"
						label="Status"
						value="{{category.status}}">
				</paper-input>
				<paper-input
						id="categoryParent"
						label="Parent"
						value="{{category.parent}}">
				</paper-input>
				<paper-input
						id="categoryRoot"
						label="Root"
						value="{{category.root}}">
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
							on-tap="_cancelCategory">
						Cancel
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax
					id="categoryAddAjax"
					content-type="application/json"
					loading="{{loading}}"
					on-response="_categoryAddResponse"
					on-error="_categoryAddError">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				value: false
			},
			category: {
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

	_cancelCategory () {
		this.$.addCategoryModal.close();
		this.category = {};
	}

	_addCategory() {
		var ajax = this.$.categoryAddAjax;
		ajax.method = "POST";
		ajax.url = "/resourceCatalogManagement/v3/resourceCategory/";
		var cat = new Object();
		if(this.category.name) {
			cat.name = this.category.name;
		}
		if(this.category.description) {
			cat.desription = this.category.description;
		}
		if(this.category.version) {
			cat.version = this.category.version;
		}
		if(this.category.class) {
			cat.baseType =  this.category.class;
		}
		if(this.category.status) {
			cat.lifecycleStatus =  this.category.status;
		}
		if(this.category.parent) {
			cat.parent =  this.category.parent;
		}
		if(this.category.root) {
			cat.root =  this.category.root;
		}
		ajax.body = JSON.stringify(cat);
		ajax.generateRequest();
	}

	_categoryAddResponse() {
		this.$.addCategoryModal.close();
		this.category = {};
		document.body.querySelector('inventory-management').shadowRoot.getElementById('categoryList').shadowRoot.getElementById('categoryGrid').clearCache();
	}

	_categoryAddError(event) {
		var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
		toast.text = event.detail.request.xhr.statusText;
		toast.open();
	}
}

window.customElements.define('category-add', categoryAdd);
