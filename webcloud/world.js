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
