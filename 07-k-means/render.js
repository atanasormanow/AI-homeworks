// TODO: this won't work, i probably need webpack or a web server
import { getPoints } from './k_means.ts'

function drawPoint(context, x, y, label, color, size) {
  if (color == null) {
    color = '#000';
  }
  if (size == null) {
    size = 5;
  }

  // to increase smoothing for numbers with decimal part
  var pointX = Math.round(x);
  var pointY = Math.round(y);

  context.beginPath();
  context.fillStyle = color;
  context.arc(pointX, pointY, size, 0 * Math.PI, 2 * Math.PI);
  context.fill();

  if (label) {
    var textX = pointX;
    var textY = Math.round(pointY - size - 3);

    context.font = 'Italic 14px Arial';
    context.fillStyle = color;
    context.textAlign = 'center';
    context.fillText(label, textX, textY);
  }
}

// Usage example:
var canvas = document.getElementById('myCanvas');
var context = canvas.getContext('2d');

const points = await getPoints();

function renderPoints(points) {
  points.forEach(({x, y}) => drawPoint(context, x, y, null, null, 3))
}

renderPoints(points);
