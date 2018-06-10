/*
 * MIT License
 * © Copyright 2016 - Geoffrey Brossard (me@geoffreybrossard.fr)
 */

d3.parliament = function() {
    /* params */
    var width,
        height,
        innerRadiusCoef = 0.4,
        scale;

    /* animations */
    var enter = {
            "smallToBig": true,
            "fromCenter": true
        },
        update = {
          'animate': true,
        },
        exit = {
            "bigToSmall": true,
            "toCenter": true
        };

    /* events */
    var parliamentDispatch = d3.dispatch("click", "dblclick", "mousedown", "mouseenter",
        "mouseleave", "mousemove", "mouseout", "mouseover", "mouseup", "touchcancel", "touchend",
        "touchmove", "touchstart");

    function parliament(data) {
        data.each(function(d) {

            // if user did not provide, fill the svg:
            width = width ? width : this.getBoundingClientRect().width;
            height = width ? width / 2 : this.getBoundingClientRect().width/2;

            var outerParliamentRadius = Math.min(width/2, height);
            var innerParliementRadius = outerParliamentRadius * innerRadiusCoef;

            /* init the svg */
            var svg = d3.select(this);

            /* compute number of seats and rows of the parliament */
            var nSeats = 0;
            d.forEach(function(p) { nSeats += (typeof p.seats === 'number') ? Math.floor(p.seats) : p.seats.length; });

            var nRows = 0;
            var maxSeatNumber = 0;
            var b = 0.5;
            (function() {
                var a = innerRadiusCoef / (1 - innerRadiusCoef);
                while (maxSeatNumber < nSeats) {
                    nRows++;
                    b += a;
                    /* NOTE: the number of seats available in each row depends on the total number
                    of rows and floor() is needed because a row can only contain entire seats. So,
                    it is not possible to increment the total number of seats adding a row. */
                    maxSeatNumber = series(function(i) { return Math.floor(Math.PI * (b + i)); }, nRows-1);
                }
            })();


            /***
             * create the seats list */
            /* compute the cartesian and polar coordinates for each seat */
            var rowWidth = (outerParliamentRadius - innerParliementRadius) / nRows;
            var seats = [];
            (function() {
                var seatsToRemove = maxSeatNumber - nSeats;
                for (var i = 0; i < nRows; i++) {
                    var rowRadius = innerParliementRadius + rowWidth * (i + 0.5);
                    var rowSeats = Math.floor(Math.PI * (b + i)) - Math.floor(seatsToRemove / nRows) - (seatsToRemove % nRows > i ? 1 : 0);
                    var anglePerSeat = Math.PI / rowSeats;
                    for (var j = 0; j < rowSeats; j++) {
                        var s = {};
                        s.polar = {
                            r: rowRadius,
                            teta: -Math.PI + anglePerSeat * (j + 0.5)
                        };
                        s.cartesian = {
                            x: s.polar.r * Math.cos(s.polar.teta),
                            y: s.polar.r * Math.sin(s.polar.teta)
                        };
                        seats.push(s);
                    }
                };
            })();

            /* sort the seats by angle */
            seats.sort(function(a,b) {
                return a.polar.teta - b.polar.teta || b.polar.r - a.polar.r;
            });

            /* fill the seat objects with data of its party and of itself if existing */
            (function() {
                var partyIndex = 0;
                var seatIndex = 0;
                seats.forEach(function(s) {
                    /* get current party and go to the next one if it has all its seats filled */
                    var party = d[partyIndex];
                    var nSeatsInParty = typeof party.seats === 'number' ? party.seats : party.seats.length;
                    if (seatIndex >= nSeatsInParty) {
                        partyIndex++;
                        seatIndex = 0;
                        party = d[partyIndex];
                    }

                    /* set party data */
                    s.party = party;
                    s.data = typeof party.seats === 'number' ? null : party.seats[seatIndex];

                    seatIndex++;
                });
            })();

            /***
             * helpers to get value from seat data */
            var seatClasses = function(d) {
                var c = "seat ";
                c += (d.party && d.party.id) || "";
                return c.trim();
            };
            var seatX = function(d) { return d.cartesian.x; };
            var seatY = function(d) { return d.cartesian.y; };
            var seatRadius = function(d) {
                var r = 0.4 * rowWidth;
                if (d.data && typeof d.data.size === 'number') {
                    r *= d.data.size;
                }
                return r;
            };

            // if scale is present, use it:
            if (scale) {
              scale.domain(d.map(function(row) { return(row.legend) }));
              var seatColor = function(d) { return scale(d.party.legend); }
            }


            /* fill svg with seats as circles */
            /* container of the parliament */
            var container = svg.select(".parliament");
            if (container.empty()) {
                container = svg.append("g");
                container.classed("parliament", true);
            }
            container.attr("transform", "translate(" + width / 2 + "," + outerParliamentRadius + ")");

            /* all the seats as circles */
            var circles = container.selectAll(".seat").data(seats);
            circles.attr("class", seatClasses);

            /* animation adding seats to the parliament */
            var circlesEnter = circles.enter().append("circle");
            circlesEnter.attr("class", seatClasses);
            circlesEnter.attr("cx", enter.fromCenter ? 0 : seatX);
            circlesEnter.attr("cy", enter.fromCenter ? 0 : seatY);
            circlesEnter.attr("r", enter.smallToBig ? 0 : seatRadius);

            if (scale) {
              circlesEnter.attr("fill", seatColor);
            }

            if (enter.fromCenter || enter.smallToBig) {
                var t = circlesEnter.transition().duration(function() { return 1000 + Math.random()*800; });
                if (enter.fromCenter) {
                    t.attr("cx", seatX);
                    t.attr("cy", seatY);
                }
                if (enter.smallToBig) {
                    t.attr("r", seatRadius);
                }
            }

            /* circles catch mouse and touch events */
            for (var evt in parliamentDispatch._) {
                (function(evt){
                    circlesEnter.on(evt, function(e) { parliamentDispatch.call(evt, this, e); });
                })(evt);
            }

            /* animation updating seats in the parliament */
            if (update.animate) {
              var circlesUpdate = circles.transition().duration(function() { return 1000 + Math.random()*800; });
            } else {
              var circlesUpdate = circles;
            }
              circlesUpdate.attr("cx", seatX)
                .attr("cy", seatY)
                .attr("r", seatRadius);

              if (scale) {
                circlesUpdate.attr("fill", seatColor);
              }

            /* animation removing seats from the parliament */
            if (exit.toCenter || exit.bigToSmall) {
                var t = circles.exit().transition().duration(function() { return 1000 + Math.random()*800; });
                if (exit.toCenter) {
                    t.attr("cx", 0).attr("cy", 0);
                }
                if (exit.bigToSmall) {
                    t.attr("r", 0);
                }
                t.remove();
            } else {
                circles.exit().remove();
            }
        });
    }

    parliament.width = function(value) {
        if (!arguments.length) return width;
        width = value;
        return parliament;
    };

    /** Deprecated since v1.0.1 */
    parliament.height = function(value) {
        if (!arguments.length) return height;
        return parliament;
    };

    parliament.innerRadiusCoef = function(value) {
        if (!arguments.length) return innerRadiusCoef;
        innerRadiusCoef = value;
        return parliament;
    };

    parliament.scale = function(value) {
      if (!arguments.length) return scale;
      scale = value;
      return parliament;
    }

    parliament.enter = {
        smallToBig: function (value) {
            if (!arguments.length) return enter.smallToBig;
            enter.smallToBig = value;
            return parliament.enter;
        },
        fromCenter: function (value) {
            if (!arguments.length) return enter.fromCenter;
            enter.fromCenter = value;
            return parliament.enter;
        }
    };

    parliament.update = {
      animate: function(value) {
        if (!arguments.length) return update.animate;
        update.animate = value;
        return parliament.update;
      }
    }

    parliament.exit = {
        bigToSmall: function (value) {
            if (!arguments.length) return exit.bigToSmall;
            exit.bigToSmall = value;
            return parliament.exit;
        },
        toCenter: function (value) {
            if (!arguments.length) return exit.toCenter;
            exit.toCenter = value;
            return parliament.exit;
        }
    };

    parliament.on = function(type, callback) {
        parliamentDispatch.on(type, callback);
    }

    return parliament;

    // util
    function series(s, n) { var r = 0; for (var i = 0; i <= n; i++) { r+=s(i); } return r; }

}
