// !preview r2d3(data = German_elections[German_elections$year == 2017, ], script = system.file('js/d3plot.js', package = 'ggparliament'), dependencies = system.file('js/d3-parliament.js', package = 'ggparliament'), d3_version = '4')
//

console.log(data);

var parliament = d3.parliament();
parliament.width(500).innerRadiusCoef(0.4);
parliament.enter.fromCenter(false).smallToBig(false);
parliament.update.animate(false);
parliament.exit.toCenter(false).bigToSmall(false);

/* Comment the lines 13-25 to use CSS: */

let manualScaleDomain = [];
let manualScaleRange = [];

for (let datum of data) {
  manualScaleDomain.push(datum.legend);
  manualScaleRange.push(datum.colour);
}

var preScale = d3.scaleOrdinal()
  .domain(manualScaleDomain)
  .range(manualScaleRange);
  
parliament.scale(preScale);

svg.datum(data).call(parliament);