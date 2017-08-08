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

  var foo = world.sites.reduce(function(acc, o) {
    var x = o.x === undefined ? 0 : o.x;
    var y = o.y === undefined ? 0 : o.y;
    acc.min = Math.min(x, y, acc.min);
    acc.max = Math.max(x, y, acc.max);
    return acc;
  }, {min: 1000000, max: -1000000});

//  var scale = 5000 / (foo.max - foo.min);
  var scale = (world.sites.length * 30) / (foo.max - foo.min);
  console.log(foo.max)
  console.log(foo.min)
  console.log(scale)

  var sites = world.sites.map(function(o) {
      var node = {
        "data": {
          "id": o.id,
          "mine": mines[o.id] === true
        },
        "position": {
          "x": o.x === undefined ? 0 : (o.x * scale),
          "y": o.y === undefined ? 0 : (o.y * scale)
        }
      };

      return node;
  });

  var optionColor = function(o) {
    var punter = o.data('option_punter')
    var move = o.data('option_move')
    return punter !== null && move < moveState ? colours[punter % colours.length] : 'black';
  };

  var isOption = function(o) {
    var punter = o.data('option_punter');
    var move = o.data('option_move');
    return punter !== null && move < moveState;
  };

  window.cyUpdate = cytoscape({
    container: document.getElementById('world'), // container to render in
    elements: sites.concat(rivers),
     style: [ // the stylesheet for the graph
      {
        selector: 'node',
        style: {
          'label': 'data(id)',
          'color': '#666',
          'font-weight': 'bold',
          'background-color': function(o) {
            return o.data('mine') ? "#de4526" : '#288119';
          }
        }
      },
       {
        selector: 'edge',
        style: {
          'width': 6,
          'mid-source-arrow-shape': function(o) {
            return isOption(o) ? 'circle' : 'none';
          },
          'mid-source-arrow-color': function(o) {
            return optionColor(o);
          },
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
