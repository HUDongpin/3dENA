#!/usr/bin/env node

/*
 * Render the homepage trajectory preview from the reviewed real-data Plotly
 * export. The analytical app keeps bootstrap uncertainty available, but this
 * small preview deliberately omits X/Y/Z error bars so the ordered centroid
 * paths remain legible at homepage scale.
 */

const fs = require("fs");
const path = require("path");

const root = path.resolve(__dirname, "..");
const sourceFile = path.join(
  root,
  "output",
  "yu-0712-e2e",
  "trajectory_3d.plotly.json"
);
const outputFile = path.join(root, "images", "trajectory-home-preview.svg");

const source = JSON.parse(fs.readFileSync(sourceFile, "utf8"));
const pathTraces = source.data.filter(
  (trace) => trace.meta && trace.meta.trajectory_role === "path"
);

if (pathTraces.length < 2) {
  throw new Error("Expected at least two real-data trajectory path traces.");
}

const width = 1200;
const height = 650;
const escapeXml = (value) =>
  String(value)
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&apos;");

// A fixed axonometric projection preserves the relationship among the three
// real centroid dimensions while keeping both short paths readable.
const project = (x, y, z) => ({
  x: 600 + 2200 * (1.2 * x - 0.8 * y),
  y: 330 + 1900 * (-1.3 * z + 0.25 * x + 0.25 * y),
});
const pointAt = (start, end, fraction) => ({
  x: start.x + (end.x - start.x) * fraction,
  y: start.y + (end.y - start.y) * fraction,
});
const number = (value) => value.toFixed(1);

const projectedTraces = pathTraces.map((trace, traceIndex) => ({
  id: `trajectory-${traceIndex + 1}`,
  name: trace.name,
  color: trace.line.color,
  points: trace.x.map((x, pointIndex) =>
    project(x, trace.y[pointIndex], trace.z[pointIndex])
  ),
}));

const pathMarkup = projectedTraces
  .map((trace) => {
    const start = trace.points[0];
    const end = trace.points[trace.points.length - 1];
    const pathData = `M ${number(start.x)} ${number(start.y)} L ${number(end.x)} ${number(end.y)}`;
    const dx = end.x - start.x;
    const dy = end.y - start.y;
    const length = Math.hypot(dx, dy);
    const perpendicular = { x: -dy / length, y: dx / length };
    const arrowPolygon = (tipFraction, baseFraction, halfWidth) => {
      const tip = pointAt(start, end, tipFraction);
      const base = pointAt(start, end, baseFraction);
      const left = {
        x: base.x + perpendicular.x * halfWidth,
        y: base.y + perpendicular.y * halfWidth,
      };
      const right = {
        x: base.x - perpendicular.x * halfWidth,
        y: base.y - perpendicular.y * halfWidth,
      };
      return `${number(tip.x)},${number(tip.y)} ${number(left.x)},${number(left.y)} ${number(right.x)},${number(right.y)}`;
    };

    return `
      <path d="${pathData}" fill="none" stroke="#ffffff" stroke-opacity="0.92"
        stroke-width="30" stroke-linecap="round" />
      <path d="${pathData}" fill="none" stroke="${trace.color}"
        stroke-width="16" stroke-linecap="round" />
      <polygon points="${arrowPolygon(0.74, 0.62, 17)}" fill="#17334b" />`;
  })
  .join("");

const nodeMarkup = projectedTraces
  .flatMap((trace) =>
    trace.points.map(
      (point, pointIndex) => `
        <g transform="translate(${number(point.x)} ${number(point.y)})">
          <circle r="31" fill="#ffffff" fill-opacity="0.96" />
          <circle r="24" fill="${trace.color}" />
          <text x="0" y="7" text-anchor="middle" fill="#ffffff"
            font-family="Arial, sans-serif" font-size="21" font-weight="800">${pointIndex + 1}</text>
        </g>`
    )
  )
  .join("");

const legendItem = (trace, x) => `
  <g transform="translate(${x} 58)">
    <line x1="0" y1="0" x2="54" y2="0" stroke="${trace.color}"
      stroke-width="11" stroke-linecap="round" />
    <circle cx="27" cy="0" r="10" fill="${trace.color}" stroke="#ffffff" stroke-width="3" />
    <text x="70" y="7" fill="#17334b" font-family="Arial, sans-serif"
      font-size="21" font-weight="700">${escapeXml(trace.name)}</text>
  </g>`;

const svg = `<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" width="${width}" height="${height}"
  viewBox="0 0 ${width} ${height}" role="img"
  aria-labelledby="trajectory-title trajectory-description">
  <title id="trajectory-title">Experimental and Control centroid trajectories</title>
  <desc id="trajectory-description">Two colored paths connect numbered centroid nodes from time one to time two, with arrows showing direction.</desc>
  <defs>
    <linearGradient id="trajectory-background" x1="0" y1="0" x2="1" y2="1">
      <stop offset="0" stop-color="#f8fcfb" />
      <stop offset="1" stop-color="#edf7f5" />
    </linearGradient>
    <radialGradient id="trajectory-focus" cx="50%" cy="48%" r="54%">
      <stop offset="0" stop-color="#ffffff" stop-opacity="0.95" />
      <stop offset="1" stop-color="#ffffff" stop-opacity="0" />
    </radialGradient>
    <filter id="trajectory-shadow" x="-20%" y="-20%" width="140%" height="140%">
      <feDropShadow dx="0" dy="9" stdDeviation="11" flood-color="#274c5a" flood-opacity="0.14" />
    </filter>
  </defs>

  <rect width="${width}" height="${height}" fill="url(#trajectory-background)" />
  <ellipse cx="600" cy="340" rx="510" ry="250" fill="url(#trajectory-focus)" />

  <g aria-label="Group legend">
    ${legendItem(projectedTraces[0], 358)}
    ${legendItem(projectedTraces[1], 638)}
  </g>

  <g filter="url(#trajectory-shadow)" aria-label="Ordered centroid paths">
    ${pathMarkup}
    ${nodeMarkup}
  </g>

  <g transform="translate(430 580)" fill="#49657a" font-family="Arial, sans-serif">
    <text x="0" y="6" font-size="13" font-weight="800" letter-spacing="2">TIME ORDER</text>
    <circle cx="142" cy="0" r="15" fill="#17334b" />
    <text x="142" y="5" text-anchor="middle" fill="#ffffff" font-size="13" font-weight="800">1</text>
    <text x="166" y="6" font-size="16" font-weight="700">Lesson 1</text>
    <path d="M 255 0 L 299 0 M 291 -7 L 299 0 L 291 7" fill="none" stroke="#79909f" stroke-width="2" />
    <circle cx="324" cy="0" r="15" fill="#17334b" />
    <text x="324" y="5" text-anchor="middle" fill="#ffffff" font-size="13" font-weight="800">2</text>
    <text x="348" y="6" font-size="16" font-weight="700">Lesson 2</text>
  </g>
</svg>
`;

fs.writeFileSync(outputFile, svg, "utf8");
process.stdout.write(`${outputFile}\n`);
