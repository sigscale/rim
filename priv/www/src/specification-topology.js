/*
 * @license
 * Copyright 2018 - 2023 SigScale Global Inc.
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
import '@polymer/paper-dialog/paper-dialog.js';
import './style-element.js';

class specificationTopology extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="topologyGraph" modal>
				<svg id="graph" />
				<div id="close-dialog" on-click="close">
					<svg viewBox="0 0 24 24" height="24" width="24">
						<path d="M0 0h24v24H0z" fill="none"/>
						<path d="M19 6.41L17.59 5 12 10.59 6.41 5 5 6.41 10.59 12 5 17.59 6.41 19 12 13.41 17.59 19 19 17.59 13.41 12z"/>
					</svg>
				</div>
			</paper-dialog>
		`;
	}

	static get properties() {
		return {
			graphSize: {
				type: Object,
				notify: true,
				readOnly: true,
				value: function() {
					return {
						width: 0,
						height: 0
					}
				}
			}
		}
	}

	connectedCallback() {
		super.connectedCallback();
		this.addEventListener('iron-resize', this.onIronResize);
	}

	disconnectedCallback() {
		super.disconnectedCallback();
		this.removeEventListener('iron-resize', this.onIronResize);
	}

	onIronResize() {
		// TODO: debounce
		this._setGraphSize({
			width: this.$.topologyGraph.clientWidth,
			height: this.$.topologyGraph.clientHeight
		});
	}

	close() {
		this.$.topologyGraph.close();
	}
}

window.customElements.define('specification-topology', specificationTopology);
