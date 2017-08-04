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
