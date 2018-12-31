const styleElement = document.createElement('dom-module');
styleElement.setAttribute('theme-for', 'vaadin-grid');

styleElement.innerHTML = `<template>
		<style>
			:host {
				@apply(--paper-font-common-base);
				--app-primary-color: #f57f17;
				--app-secondary-color: #aeea00;
				display: block;
			}
			app-header {
				position: fixed;
				top: 0;
				left: 0;
				width: 100%;
				text-align: center;
				background-color: var(--app-primary-color);
				border-bottom: 1px solid #eee;
				color: #fff;
			}
			.toolbar-top {
				background-color: var(--app-primary-color);
			}
			app-header paper-icon-button {
				--paper-icon-button-ink-color: white;
			}
			app-drawer {
				--app-drawer-content-container: {
					padding-top: 10px;
				};
				height: 100%;
				top: 64px;
			}
			paper-progress {
				display: block;
				width: 100%;
				--paper-progress-active-color: var(--paper-lime-a700);
				--paper-progress-container-color: transparent;
			}
			.drawer-list {
				box-sizing: border-box;
				width: 100%;
				height: 100%;
				padding: 10px;
				background: white;
				position: relative;
			}
			.drawer-list a {
				display: block;
				padding: 0 24px;
				text-decoration: none;
				color: black;
				line-height: 40px;
			}
			.drawer-list a.iron-selected {
				color: #78909C;
				font-weight: bold;
			}
			#restError {
				--paper-toast-background-color: var(--paper-red-a400);
			}
			paper-dropdown-menu {
				--paper-input-container-label: {
					font-size: 24px;
				};
				--paper-input-container-input: {
					font-size: 24px;
					font-weight: 400;
				};
			}
			.grouptitle {
				text-align: center;
				border-bottom-style: solid;
				border-color: var(--paper-yellow-900);
			}
			vaadin-grid {
				height: 100vh;
				font-size: inherit;
			}
			vaadin-grid input {
				font-size: initial;
				border-style: none;
				background: #ffb04c;
				max-width: 130px;
			}
			vaadin-grid input::placeholder {
				color: black;
            font-weight: bold;
            font-size: inherit;
			}
			[part~="header-cell"] {
				background-color: #ffb04c;
			}
			paper-card {
				margin: 4px;
				vertical-align: top;
			}
			paper-icon-item {
				--paper-item-icon-width: 32px;
				--paper-item-min-height: 1em;
			}
			.labelName {
				font-style: normal;
				font-weight: 700;
			}
		</style>
	</template>`;

styleElement.register('style-element');

