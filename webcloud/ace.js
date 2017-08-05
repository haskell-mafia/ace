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

var moves2 = moves.reduce(function(acc, s, i) {
  if (s.claim !== undefined) {
    var c = s.claim;
    var id = c.source + ":" + c.target;
    acc[id] = {};
    acc[id].punter = c.punter;
    acc[id].move = i;
  }
  return acc;
}, {});

var punters = moves.reduce(function(acc, s) {
  if (s.claim !== undefined) {
    var c = s.claim;
    acc[c.punter] = acc[c.punter] !== undefined ? acc[c.punter] + 1 : 0;
  }
  return acc;
}, {});

var punterCount = Object.keys(punters).length;

var rivers = world.rivers.map(function(o) {
  var id = o.source.toString() + ":" + o.target.toString();
  var edge = {
    "data": {
      "id": id,
      "source": o.source,
      "target": o.target,
      "punter": moves2[id] ? moves2[id].punter : undefined,
      "move": moves2[id] ? moves2[id].move : undefined
    }
  };

  return edge;
});

var colours = [];
colours[0] = 'aqua';
colours[1] = 'blue';
colours[2] = 'fuchsia';
colours[3] = 'gray';
colours[4] = 'lime';
colours[5] = 'maroon';
colours[6] = 'navy';
colours[7] = 'olive';
colours[8] = 'purple';
colours[9] = 'yellow';
colours[10] = 'silver';
colours[11] = 'teal';

if (window.player !== undefined) {
  colours[window.player] = 'red';
}

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
          'line-color': function(o) {
            var punter = o.data('punter')
            var move = o.data('move')
            return (punter !== null && move < moveG ? colours[punter % colours.length] : 'black');
          }
        }
      }
    ],

    layout: {
      name: preset ? 'preset' : 'cose'
    }
  });
  var elMove = document.querySelector('#move');
  var elNext = document.querySelector('#next');
  var elPrev = document.querySelector('#prev');
  var moveG = moves.length;
  var refresh = function(move) {
    moveG = Math.max(0, Math.min(moves.length, move));
    elMove.innerHTML = moveG.toString();
    cy.json({elements: sites.concat(rivers)});
  };
  elNext.onclick = function() {
    refresh(moveG + 1);
  };
  elPrev.onclick = function() {
    refresh(moveG - 1);
  };
  refresh(moveG);
};
