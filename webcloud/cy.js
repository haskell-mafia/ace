window.cy = function(world, rivers) {
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

  var colours = [
    '#a6cee3',
    '#b2df8a',
    '#ff7f00',
    '#6a3d9a',
    '#1f78b4',
    '#33a02c',
    '#fb9a99',
    '#fdbf6f',
    '#cab2d6'
  ]

  window.cyUpdate = cytoscape({
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
          'width': 6,
          'line-color': function(o) {
            var punter = o.data('punter')
            var move = o.data('move')
            return (punter !== null && move < moveState ? colours[punter % colours.length] : 'black');
          }
        }
      }
    ],

      layout: {
      name: preset ? 'preset' : 'cose-bilkent'
    }
  });

  cyUpdate.json({elements: sites.concat(rivers)});

  return sites;

}
