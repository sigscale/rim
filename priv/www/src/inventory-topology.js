
import { PolymerElement, html } from '@polymer/polymer/polymer-element.js';
import '@polymer/paper-dialog/paper-dialog.js';
import './style-element.js';

class inventoryTopology extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="topologyGraph" opened={{modalOpen}} modal>
				<div id="close-dialog" on-click="_close">
					<svg viewBox="0 0 24 24" height="24" width="24">
						<path d="M0 0h24v24H0z" fill="none"/>
						<path d="M19 6.41L17.59 5 12 10.59 6.41 5 5 6.41 10.59 12 5 17.59 6.41 19 12 13.41 17.59 19 19 17.59 13.41 12z"/>
					</svg>
				</div>
				<div id="topology-graph">
					<svg width="100%", height="100%" />
				</div>
			</paper-dialog>
		`;
	}

	static get properties() {
		return {
			modalOpen: {
				type: Boolean,
				notify: true
			}
		}
	}

	ready() {
		super.ready()
	}

	_close() {
		this.$.topologyGraph.close();
	}
}

window.customElements.define('inventory-topology', inventoryTopology);
window.addEventListener('load', (event) => {
	console.log('page is fully loaded');
});
