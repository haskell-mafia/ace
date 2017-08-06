var colours = [
  '#41D3BD',
  '#BC96E6',
  '#DE6449',
  '#407899',
  '#3E5641',
  '#A24936',
  '#282B28',
  '#E98A15',
  '#83BCA9',
  '#F0A202',
  '#96E6B3',
  '#A3D9FF',
  '#DA3E52',
  '#7E6B8F'
]

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
