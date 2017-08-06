var fetchMoves = function(filename, cb) {
  var xhttp = new XMLHttpRequest();
  xhttp.onreadystatechange = function() {
    if (this.readyState === 4 && this.status === 200) {
      cb([].concat.apply([], this.responseText.split('\n')
        .filter(function(x) { return x !== '' })
        .map(function(j) { return JSON.parse(j); })
        ));
    }
  };
  xhttp.open("GET", filename + "?rnd=" + Math.random(), true);
  xhttp.send();
};

var fetchWorld = function(filename, cb) {
  var xhttp = new XMLHttpRequest();
  xhttp.onreadystatechange = function() {
    if (this.readyState === 4 && this.status === 200) {
      cb(JSON.parse(this.responseText));
    }
  };
  xhttp.open("GET", filename + "?rnd=" + Math.random(), true);
  xhttp.send();
};

window.savedWorld = 0;
window.savedSites = 0;
window.elCurrentMove = document.querySelector('#move');
window.elTotalMoves = document.querySelector('#total');
window.elNext = document.querySelector('#next');
window.elPrev = document.querySelector('#prev');
window.elPlay = document.querySelector('#play');
window.elSpeed = document.querySelector('#speed');
window.elDir = document.querySelector('#dir');
window.moveState = 0;
window.totalMoves = 0;

window.changeMoves = function(n) {
  var p = moveState + n
  var i = Math.max(0, Math.min(totalMoves, p));
  moveState = i
  elCurrentMove.value = moveState.toString();
}

window.changeTotal = function(tot) {
  totalMoves = tot
  elTotalMoves.innerHTML = tot.toString();
}

window.nextMove = function() { changeMoves(+1); }
window.prevMove = function() { changeMoves(-1); }

window.setTotal = function(moves) {
  changeTotal(moves.length);
}

window.calculateRivers = function(world, moves) {
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

  return world.rivers.map(function(o) {
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
};

window.doMoveThings = function(setup, sites, moves) {
  setTotal(moves);
  var rivers = calculateRivers(setup.world, moves);
  cyUpdate.json({elements: sites.concat(rivers)});
}

window.refresh = function(setup, sites) {
  fetchMoves(elDir.value + "/moves.txt", function(moves){
    doMoveThings(setup, sites, moves)
  })
};

elCurrentMove.onkeydown = function(e) {
  if (e.keyCode === 13) {
    refresh(savedWorld, savedSites);
    moveState = parseInt(elCurrentMove.value)
    elCurrentMove.value = moveState.toString();
  }
};

elNext.onclick = function() {
  nextMove();
  refresh(savedWorld, savedSites);
};

elPrev.onclick = function() {
  prevMove();
  refresh(savedWorld, savedSites);
};

elPlay.onclick = function() { play(); };

var play = function() {
  setTimeout( function(){
    nextMove();
    refresh(savedWorld, savedSites);
    if (moveState < totalMoves) {
      play();
    }
  }, (elSpeed.value * 1000))
}

var gameLocation = function() {
  var url = window.location.search;
  dir = url.replace("?game=", '');
  if (dir != '') {
    elDir.value = dir
  }
}()

window.run = function() {
  fetchWorld(elDir.value + "/world.json", function(setup) {
    var sites = cy(setup.world, calculateRivers(setup.world, []));
    savedWorld = setup;
    savedSites = sites;
    refresh(setup, sites);
  })
}();
