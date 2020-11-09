/**
 * @license
 * Copyright (c) 2020 The Polymer Project Authors. All rights reserved.
 * This code may only be used under the BSD style license found at http://polymer.github.io/LICENSE.txt
 * The complete set of authors may be found at http://polymer.github.io/AUTHORS.txt
 * The complete set of contributors may be found at http://polymer.github.io/CONTRIBUTORS.txt
 * Code distributed by Google as part of the polymer project is also
 * subject to an additional IP rights grant found at http://polymer.github.io/PATENTS.txt
 */

import { PolymerElement, html } from '@polymer/polymer/polymer-element.js';
import {} from '@polymer/polymer/lib/elements/dom-if.js';
import {} from '@polymer/polymer/lib/elements/dom-repeat.js';
import '@polymer/iron-ajax/iron-ajax.js';
import '@polymer/paper-fab/paper-fab.js';
import '@vaadin/vaadin-grid/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid-filter.js';
import '@vaadin/vaadin-grid/vaadin-grid-sorter.js';
import '@polymer/paper-button/paper-button.js';
import './style-element.js';

class specificationList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="specificationGrid"
					loading="{{loading}}"
					active-item="{{activeItem}}">
				<template class="row-details">
					<dl class="details">
						<template is="dom-if" if="{{item.id}}">
							<dt><b>Id</b></dt>
							<dd>{{item.id}}</dd>
						</template>
						<template is="dom-if" if="{{item.name}}">
							<dt><b>Name</b></dt>
							<dd>{{item.name}}</dd>
						</template>
						<template is="dom-if" if="{{item.description}}">
							<dt><b>Description</b></dt>
							<dd>{{item.description}}</dd>
						</template>
						<template is="dom-if" if="{{item.category}}">
							<dt><b>Category</b></dt>
							<dd>{{item.category}}</dd>
						</template>
						<template is="dom-if" if="{{item.isBundle}}">
							<dt><b>Isbundle</b></dt>
							<dd>{{item.isBundle}}</dd>
						</template>
						<template is="dom-if" if="{{item.lifecycleStatus}}">
							<dt><b>Status</b></dt>
							<dd>{{item.lifecycleStatus}}</dd>
						</template>
						<template is="dom-if" if="{{item.lifecycleStatus}}">
							<dt><b>Status</b></dt>
							<dd>{{item.lifecycleStatus}}</dd>
						</template>
						<template is="dom-if" if="{{item.version}}">
							<dt><b>Version</b></dt>
							<dd>{{item.version}}</dd>
						</template>
					</dl>
					<h3 class="specificationDetail">Resource Specification Characteristics:</h3>
					<dl>
						<template is="dom-if" if="{{item.resourceSpecCharacteristic}}">
							<template is="dom-repeat" items="{{item.resourceSpecCharacteristic}}" as="detail">
								<dt>description</dt>
								<dd>{{detail.description}}</dd>
								<dt>name</dt>
								<dd>{{detail.name}}</dd>
								<dt>valueType</dt>
								<dd>{{detail.valueType}}</dd>
							</template>
						</template>
					</dl>
					<template is="dom-if" if="{{item.connectivitySpecification}}"
							on-dom-change="showInlineGraphSpec">
						<h3 class="inventoryDetail">Connectivity Specification:</h3>
						<svg id$="graphSpec[[item.id]]" width="100%" />
					</template>
					<div class="buttons">
						<paper-button
								raised
								class="submit-button"
								on-tap="_showupdateSpecificationModal">
							Update
						</paper-button>
					</div>
				</template>
				<vaadin-grid-column width="8ex" flex-grow="2">
					<template class="header">
						<vaadin-grid-sorter
								path="name">
							<vaadin-grid-filter
									id="filterSpecName"
									aria-label="Name"
									path="name"
									value="{{_filterSpecName}}">
								<input
										slot="filter"
										placeholder="Name"
										value="{{_filterSpecName::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>[[item.name]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="20ex" flex-grow="5">
					<template class="header">
						<vaadin-grid-sorter
								path="decription">
							<vaadin-grid-filter
									id="filterSpecDesc"
									aria-label="Description"
									path="description"
									value="{{_filterSpecDesc}}">
								<input
									slot="filter"
									placeholder="Description"
									value="{{_filterSpecDesc::input}}"
									focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>[[item.description]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="12ex" flex-grow="1">
					<template class="header">
						<vaadin-grid-sorter
								path="@type">
							<vaadin-grid-filter
									id="filterSpecClass"
									aria-label="Class"
									path="@type"
									value="{{_filterSpecClass}}">
								<input
									slot="filter"
									placeholder="Class"
									value="{{_filterSpecClass::input}}"
									focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>
						[[item.type]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="8ex" flex-grow="1">
					<template class="header">
						<vaadin-grid-sorter
								path="lifecycleStatus">
							<vaadin-grid-filter
									id="filterSpecStatus"
									aria-label="Status"
									path="lifecycleStatus"
									value="{{_filterSpecStatus}}">
								<input
									slot="filter"
									placeholder="Status"
									value="{{_filterSpecStatus::input}}"
									focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>
						[[item.status]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="8ex" flex-grow="1">
					<template class="header">
						<vaadin-grid-sorter
								path="category">
							<vaadin-grid-filter
									id="filterSpecCat"
									aria-label="Category"
									path="category"
									value="{{_filterSpecCategory}}">
								<input
									slot="filter"
									placeholder="Category"
									value="{{_filterSpecCategory::input}}"
									focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>
						[[item.category]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="6ex" flex-grow="1">
					<template class="header">
						<vaadin-grid-sorter
								path="isBundle">
							<vaadin-grid-filter
									id="filterSpecBundle"
									aria-label="Bundle"
									path="isBundle"
									value="{{_filterSpecBundle}}">
								<input
									slot="filter"
									placeholder="Bundle"
									value="{{_filterSpecBundle::input}}"
									focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>
						[[item.bundle]]
					</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<div class="add-button">
				<paper-fab
						icon="add"
						on-tap="_showAddSpecificationModal">
				</paper-fab>
			</div>
			<iron-ajax
				id="specificationGetAjax"
				url="resourceCatalogManagement/v4/resourceSpecification"
				rejectWithRequest>
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				notify: true
			},
			etag: {
				type: String,
				value: null
			},
			activeItem: {
				type: Object,
				notify: true,
				observer: '_activeItemChanged'
			},
			_filterSpecName: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterSpecDesc: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterSpecClass: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterSpecStatus: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterSpecCategory: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterSpecBundle: {
				type: Boolean,
				observer: '_filterChanged'
			}
		}
	}

   _activeItemChanged(item, last) {
      if(item || last) {
         var grid = this.$.specificationGrid;
         var current;
         if(item == null) {
            current = last;
				this.$.specificationGrid.selectedItems = item ? [item] : [];
         } else {
            current = item;
				this.$.specificationGrid.selectedItems = [];
         }
         function checkExist(specification) {
            return specification.id == current.id;
         }
         if(grid.detailsOpenedItems && grid.detailsOpenedItems.some(checkExist)) {
            grid.closeItemDetails(current);
         } else {
            grid.openItemDetails(current);
         }
      }
   }
//	_activeItemChanged(item) {
//		if(item) {
//			this.$.specificationGrid.selectedItems = item ? [item] : [];
  //    } else {
//			this.$.specificationGrid.selectedItems = [];
//		}
//	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('specificationGrid');
		grid.dataProvider = this._getSpecification;
	}

	_getSpecification(params, callback) {
		var grid = this;
		if(!grid.size) {
				grid.size = 0;
		}
		var specificationList = document.body.querySelector('inventory-management').shadowRoot.querySelector('specification-list');
		var ajax = specificationList.shadowRoot.getElementById('specificationGetAjax');
		delete ajax.params['filter'];
		var query = "";
		params.filters.forEach(function(filter) {
			if(filter.path != "isBundle") {
				if(filter.value) {
					if (query) {
						query = query + "," + filter.path + ".like=[" + filter.value + "%]";
					} else {
						query = "[{" + filter.path + ".like=[" + filter.value + "%]";
					}
				}
			} else if(filter.path == "lifecycleStatus") {
				if(filter.value) {
					if("Obsolete".startsWith(filter.value)) {
						if (query) {
							query = query + ",lifecycleStatus=Obsolete";
						} else {
							query = "[{lifecycleStatus=Obsolete";
						}
					} else if("Launched".startsWith(filter.value)) {
						if (query) {
							query = query + ",lifecycleStatus=Launched";
						} else {
							query = "[{lifecycleStatus=Launched";
						}
					} else if("Active".startsWith(filter.value)) {
						if (query) {
							query = query + ",lifecycleStatus=Active";
						} else {
							query = "[{lifecycleStatus=Active";
						}
					} else if("In ".startsWith(filter.value)) {
						if (query) {
							query = query + ",lifecycleStatus.in=[In Study,In Design, In Test]";
						} else {
							query = "[{lifecycleStatus.in=[In Study,In Design, In Test]";
						}
					} else if("In Study".startsWith(filter.value)) {
						if (query) {
							query = query + ",lifecycleStatus=In Study";
						} else {
							query = "[{lifecycleStatus=In Study";
						}
					} else if("In Design".startsWith(filter.value)) {
						if (query) {
							query = query + ",lifecycleStatus=In Design";
						} else {
							query = "[{lifecycleStatus=In Design";
						}
					} else if("Re".startsWith(filter.value)) {
						if (query) {
							query = query + ",lifecycleStatus.in=[Rejected,Retired]";
						} else {
							query = "[{lifecycleStatus.in=[Rejected,Retired]";
						}
					} else if("Rejected".startsWith(filter.value)) {
						if (query) {
							query = query + ",lifecycleStatus=Rejected";
						} else {
							query = "[{lifecycleStatus=Rejected";
						}
					} else if("Retired".startsWith(filter.value)) {
						if (query) {
							query = query + ",lifecycleStatus=Retired";
						} else {
							query = "[{lifecycleStatus=Retired";
						}
					} else {
						if (query) {
							query = query + ",lifecycleStatus=" + filter.value;
						} else {
							query = "[{lifecycleStatus=" + filter.value;
						}
					}
				}
			} else {
				if(filter.value) {
					if("true".startsWith(filter.value)) {
						if (query) {
							query = query + ",isBundle=true";
						} else {
							query = "[{isBundle=true";
						}
					} else if("false".startsWith(filter.value)) {
						if (query) {
							query = query + ",isBundle=false";
						} else {
							query = "[{isBundle=false";
						}
					} else {
						if (query) {
							query = query + ",isBundle=" + filter.value;
						} else {
							query = "[{isBundle=" + filter.value;
						}
					}
				}
			}
		});
		if(query) {
			ajax.params['filter'] = "\"" + query + "}]\"";
		}
		if(specificationList.etag && params.page > 0) {
			ajax.headers['If-Range'] = specificationList.etag;
		}
		var handleAjaxResponse = function(request) {
			if(request) {
				specificationList.etag = request.xhr.getResponseHeader('ETag');
				var range = request.xhr.getResponseHeader('Content-Range');
				var range1 = range.split("/");
				var range2 = range1[0].split("-");
				if (range1[1] != "*") {
					grid.size = Number(range1[1]);
				} else {
					grid.size = Number(range2[1]) + grid.pageSize * 2;
				}
				var vaadinItems = new Array();
				for(var index in request.response) {
					var newRecord = new Object();
					newRecord.id = request.response[index].id;
					newRecord.name = request.response[index].name;
					newRecord.description = request.response[index].description;
					newRecord.type = request.response[index]["@type"];
					newRecord.base = request.response[index]["@baseType"];
					newRecord.status = request.response[index].lifecycleStatus;
					newRecord.category = request.response[index].category;
					newRecord.bundle = request.response[index].isBundle;
					newRecord.feature = request.response[index].resourceSpecFeature;
					newRecord.resourceSpecCharacteristic = request.response[index].resourceSpecCharacteristic;
					vaadinItems[index] = newRecord;
				}
				callback(vaadinItems);
			} else {
				callback([]);
			}
		};
		var handleAjaxError = function(error) {
			specificationList.etag = null;
			var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
			toast.text = error;
			toast.open();
			callback([]);
		}
		if(ajax.loading) {
			ajax.lastRequest.completes.then(function(request) {
				var startRange = params.page * params.pageSize + 1;
				ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
					if (specificationList.etag && params.page > 0) {
						ajax.headers['If-Range'] = specificationList.etag;
					} else {
						delete ajax.headers['If-Range'];
					}
					return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (specificationList.etag && params.page > 0) {
				ajax.headers['If-Range'] = specificationList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	_filterChanged(filter) {
		this.etag = null;
		var grid = this.shadowRoot.getElementById('specificationGrid');
		grid.size = 0;
	}

	_showupdateSpecificationModal(event) {
		document.body.querySelector('inventory-management').shadowRoot.querySelector('specification-update').shadowRoot.getElementById('specificationUpdateModal').open();
	}

	_showAddSpecificationModal(event) {
		document.body.querySelector('inventory-management').shadowRoot.querySelector('specification-add').shadowRoot.getElementById('specificationAddModal').open();
	}
}
/*   showInlineGraphSpec(event) {
      var grid = this.$.inventoryGrid;
      var connectivitySpec = event.model.item.connectivitySpecification;
      var gridGraph = grid.querySelector('#graphSpec' + event.model.item.id);
      var width = gridGraph.clientWidth;
      var height = gridGraph.clientHeight;
      var graph = select(this.$.inventoryGrid)
            .select('#graphSpec' + event.model.item.id);
      _connectivitySpecGraph(connectivitySpec, graph, width, height);
   }
}

function _connectivitySpecGraph(connectivitySpec, graph, width, height) {
   var vertices = [];
   function mapEdge(connectivitySpecification) {
      let edge = {};
      if(connectivitySpecification.id) {
         edge.id = connectivitySpecification.id;
      }
      if(connectivitySpecification.name) {
         edge.name = connectivitySpecification.name;
      }
      if(connectivitySpecification.description) {
         edge.description = connectivitySpecification.description;
      }
      if(connectivitySpecification.connectionSpecification) {
         let index = vertices.findIndex(function (vertex) {
            return vertex.id == connectivitySpecification.connectionSpecification[0].id;
         });
         if(index == -1) {
            let v0 = {};
            v0.id = connectivitySpecification.connectionSpecification[0].id;
            if(connectivitySpecification.connectionSpecification[0].name) {
               v0.name = connectivitySpecification.connectionSpecification[0].name;
            }
            if(connectivitySpecification.connectionSpecification[0].description) {
               v0.description = connectivitySpecification.connectionSpecification[0].description;
            }
            if(connectivitySpecification.connectionSpecification[0].associationType) {
               v0.associationType = connectivitySpecification.connectionSpecification[0].associationType;
            }
            if(connectivitySpecification.connectionSpecification[0].endpointSpecification) {
               var endPoint = connectivitySpecification.connectionSpecification[0];
               let index1 = vertices.findIndex(function (vertex1) {
                  return vertex1.id == endPoint.endpointSpecification[0].id;
               });
               if(index1 == -1) {
                  let v1 = {};
                  v1.id = endPoint.endpointSpecification[0].id;
                  if(endPoint.endpointSpecification[0].href) {
                     v1.href = endPoint.endpointSpecification[0].href;
                  }
                  if(endPoint.endpointSpecification[0]["@referredType"]) {
                     v1.type = endPoint.endpointSpecification[0]["@referredType"];
                  }
                  if(endPoint.endpointSpecification[0].name) {
                     v1.name = endPoint.endpointSpecification[0].name;
                  }
                  edge.source = vertices.push(v1) - 1;
               } else {
                  edge.source = index1;
               }
               index1 = vertices.findIndex(function (vertex1) {
                  return vertex1.id == endPoint.endpointSpecification[1].id;
               });
               if(index1 == -1) {
                  let v2 = {};
                  v2.id = endPoint.endpointSpecification[1].id;
                  if(endPoint.endpointSpecification[1].href) {
                     v2.href = endPoint.endpointSpecification[1].href;
                  }
                  if(endPoint.endpointSpecification[1]["@referredType"]) {
                     v2.type = endPoint.endpointSpecification[1]["@referredType"];
                  }
                  if(endPoint.endpointSpecification[1].name) {
                     v2.name = endPoint.endpointSpecification[1].name;
                  }
                  if(endPoint.endpointSpecification[1].connectionPointSpecification.id) {
                     v2.pointId = endPoint.endpointSpecification[1].connectionPointSpecification.id;
                  }
                  if(endPoint.endpointSpecification[1].connectionPointSpecification.href) {
                     v2.pointHref = endPoint.endpointSpecification[1].connectionPointSpecification.href;
                  }
                  if(endPoint.endpointSpecification[1].connectionPointSpecification["@referredType"]) {
                     v2.pointType = endPoint.endpointSpecification[1].connectionPointSpecification["@referredType"];
                  }
                  if(endPoint.endpointSpecification[1].connectionPointSpecification.name) {
                     v2.pointName = endPoint.endpointSpecification[1].connectionPointSpecification.name;
                  }
                  edge.target = vertices.push(v2) - 1;
               } else {
                  edge.target = index1;
               }
            }
         }
      }
      return edge;
   }
   var edges = connectivitySpec.map(mapEdge);
   var simulation = forceSimulation(vertices)
         .force("center", forceCenter(Math.ceil(width/2), Math.ceil(height/2)))
         .force("charge", forceManyBody().strength(-800))
         .force("link", forceLink(edges).distance(100))
         .force('y', forceY());
   var edge = graph.selectAll('.edge')
         .data(edges)
         .enter()
         .append('line')
               .attr('class', 'edge');
   var vertex1 = graph.selectAll('g.vertex1')
         .data(vertices);
   var vgroup = vertex1.enter()
         .append('g')
   var circle = vgroup.append('circle')
         .attr('r', Math.ceil(width / 100))
         .attr('class', 'vertex1')
         .append('title')
         .text(function(d) { return d.name});
   vgroup.append('text')
         .text(function(d) { return d.type })
         .attr('y', - Math.ceil(width / 100) - 8 )
         .attr('text-anchor', 'middle');
   simulation.on('tick', function() {
      vgroup.attr('transform', function(d) {
            return 'translate(' + Math.ceil(d.x) + ',' + Math.ceil(d.y) + ')';
      });
      edge.attr('x1', function(d) { return Math.ceil(d.source.x) })
            .attr('y1', function(d) { return Math.ceil(d.source.y) })
            .attr('x2', function(d) { return Math.ceil(d.target.x) })
            .attr('y2', function(d) { return Math.ceil(d.target.y) });
   });
}*/

window.customElements.define('specification-list', specificationList);
