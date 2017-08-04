var world = {
  "sites": [
    { "id": 0, "x": 0.0, "y": 0.0 },
    { "id": 1, "x": 100.0, "y": 0.0 },
    { "id": 2, "x": 200.0, "y": 0.0 },
    { "id": 3, "x": 200.0, "y": -100.0 },
    { "id": 4, "x": 200.0, "y": -200.0 },
    { "id": 5, "x": 100.0, "y": -200.0 },
    { "id": 6, "x": 0.0, "y": -200.0 },
    { "id": 7, "x": 0.0, "y": -100.0 }
  ],
  "rivers": [
    { "source": 0, "target": 1 },
    { "source": 1, "target": 2 },
    { "source": 0, "target": 7 },
    { "source": 7, "target": 6 },
    { "source": 6, "target": 5 },
    { "source": 5, "target": 4 },
    { "source": 4, "target": 3 },
    { "source": 3, "target": 2 },
    { "source": 1, "target": 7 },
    { "source": 1, "target": 3 },
    { "source": 7, "target": 5 },
    { "source": 5, "target": 3 }
  ],
  "mines": [1, 5]
};

var preset = world.sites.reduce(function(acc, o) {
  return acc && o.x !== undefined && o.y !== undefined;
}, true);

var mines = world.mines.reduce(function(acc, n) {
  acc[n] = true;
  return acc;
}, {});

var sites = world.sites.map(function(o) {
  var node = {
    "data": {
      "id": o.id,
      "mine": mines[o.id] === true
    },
    "position": {
      "x": o.x === undefined ? 0 : o.x,
      "y": o.y === undefined ? 0 : o.y
    }
  };

  return node;
});

var rivers = world.rivers.map(function(o) {
  var edge = {
    "data": {
      "id": o.source.toString() + ":" + o.target.toString(),
      "source": o.source,
      "target": o.target
    }
  };

  return edge;
});

window.onload = function() {
  var cy = cytoscape({
    container: document.getElementById('world'), // container to render in
    elements: sites.concat(rivers),

    style: [ // the stylesheet for the graph
      {
        selector: 'node',
        style: {
          'label': 'data(id)',
          'background-color': function(o) {
            return o.data('mine') ? "#de4526" : '#288119';
          }
        }
      },

      {
        selector: 'edge',
        style: {
          'width': 3,
          'line-color': '#3476a7'
        }
      }
    ],

    layout: {
      name: preset ? 'preset' : 'cose'
    }
  });
};
