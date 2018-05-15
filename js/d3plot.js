// !preview r2d3(data = German_elections[German_elections$year == 2017, ], script = system.file('js/d3plot.js', package = 'ggparliament'), dependencies = system.file('js/d3-parliament.js', package = 'ggparliament'), d3_version = '4')
//

var parliament = d3.parliament();
parliament.width(500).height(500).innerRadiusCoef(0.4);
parliament.enter.fromCenter(true).smallToBig(true);
parliament.exit.toCenter(false).bigToSmall(true);

d3.select("svg").datum(data).call(parliament);