/**
 * Inspired from bunkat's supercool lane views using d3 js
 * http://bl.ocks.org/1962173 
 * https://gist.github.com/2338034*
 */

//draw the timeline
//

function drawTimeline(){
  var pdata = profileData();

  var res = pdata.res;
  var duration = pdata.duration;
  var start = pdata.start;
  var kernels = pdata.kernels;
  var loc = pdata.location;
  var line_in_source = pdata.line_in_source;
  var cssline_in_source = pdata.cssline_in_source;
  var tooltip = pdata.tooltip;

  var title_bar_width = 64; // the bar with the names of the taskRunners/threads
  var main_row_height = 50;
  var mini_row_height = 25; // height of the bars
  var total_duration = totalDuration(duration, start, location); // the total duration of the computation, in us
  //var w = 1000;//1600; // width of the entire chart, in pixels
  var task_width = .96 * main_row_height;
  var numThreads = res.length;

  var space_on_top = 20;
  var space_for_ticks = 20;
  var task_offset = space_on_top + 1;
  var numTicks = 20;

  //var miniHeight = numThreads * mini_row_height + 50;
  //var mainHeight = numThreads * main_row_height + 50;

  var m = [20, 15, 15, 120], //top right bottom left
      w = 1000 - m[1] - m[3],
      h = 400 - m[0] - m[2],
      miniHeight = numThreads * 12 + 50,
      mainHeight = numThreads * 40 + 50; //h - miniHeight - 50;

  //scales
  var x = d3.scale.linear()
    .domain([0, total_duration])
    .range([0, w]);
  var x1 = d3.scale.linear()
    .range([0, w]);
  var y1 = d3.scale.linear()
    .domain([0, numThreads])
    .range([0, mainHeight]);
  var y2 = d3.scale.linear()
    .domain([0, numThreads])
    .range([0, miniHeight]);

  var chart = d3.select("#svg")
     .append("svg")
     .attr("class", "chart")
     .attr("width", w + m[1] + m[3])//title_bar_width)
     .attr("height", h + m[0] + m[2]);//mainHeight + miniHeight + space_on_top + space_for_ticks);

  chart.append("defs").append("clipPath")
    .attr("id", "clip")
    .append("rect")
    .attr("width", w) 
    .attr("height", mainHeight);

  var main = chart.append("g")
    .attr("transform", "translate(" + m[3] + "," + m[0] + ")")
    .attr("width", w)
    .attr("height", mainHeight)
    .attr("class", "main");

  var mini = chart.append("g")
    .attr("transform", "translate(" + m[3] + "," + (mainHeight + 3 * m[0]) + ")")
    .attr("width", w)
    .attr("height", miniHeight)
    .attr("class", "mini");

  //main lanes and texts
  main.append("g").selectAll(".laneLines")
    .data(res)
    .enter().append("line")
    .attr("x1", m[1])
    .attr("y1", function(d,i) {return y1(i);})
    .attr("x2", w)
    .attr("y2", function(d,i) {return y1(i);})
    .attr("stroke", "lightgray")

  main.append("g").selectAll(".laneText")
    .data(res)
    .enter().append("text")
    .text(String)
    .attr("x", -m[1])
    .attr("y", function(d, i) {return y1(i + .5);})
    .attr("dy", ".5ex")
    .attr("text-anchor", "end")
    .attr("class", "laneText");

  //mini lanes and texts
  mini.append("g").selectAll(".laneLines")
    .data(res)
    .enter().append("line")
    .attr("x1", m[1])
    .attr("y1", function(d,i) {return y2(i);})
    .attr("x2", w)
    .attr("y2", function(d,i) {return y2(i);})
    .attr("stroke", "lightgray");

  mini.append("g").selectAll(".laneText")
    .data(res)
    .enter().append("text")
    .text(String)
    .attr("x", -m[1])
    .attr("y", function(d, i) {return y2(i + .5);})
    .attr("dy", ".5ex")
    .attr("text-anchor", "end")
    .attr("class", "laneText");

  /*the axes for the main and mini windows*/
  var xMiniAxis = d3.svg.axis()
    .scale(x)
    .orient('bottom')
    //.ticks(20)
    //.tickFormat('us')
    .tickSize(6, 0, 0);

  var xMainAxis = d3.svg.axis()
    .scale(x1)
    .orient('bottom')
    //.ticks(20)
    //.tickFormat
    .tickSize(6, 0, 0);

  //append the smaller axis to the main window
  main.append('g')
    .attr('transform', 'translate(0,' + mainHeight + ')')
    .attr('class', 'main axis date')
    .call(xMainAxis);

   //append axis to the mini window
   mini.append('g')
    .attr('transform', 'translate(0,' + miniHeight + ')')
    .call(xMiniAxis);

  var itemRects = main.append("g")
    .attr("clip-path", "url(#clip)");

  //mini item rects
  mini.append("g").selectAll("miniItems")
    .data(duration)
    .enter().append("rect")
    .attr("class", "whatev")
    .attr("x", function(d, i) {return x(start[i]);})
    .attr("y", function(d, i) {return y2(loc[i] + .5) - 5;})
    .attr("width", function(d, i) {return x(duration[i]);})
    .attr("height", 10);//

  //brush
  var brush = d3.svg.brush()
    .x(x)
    .extent([total_duration/2 - 100000, total_duration/2 + 100000])
    .on("brush", display);

  mini.append("g")
    .attr("class", "x brush")
    .call(brush)
    .selectAll("rect")
    .attr("y", 1)
    .attr("height", miniHeight - 1);

  display();

  function getVisIndices(min, max){
    res = [];
    for(i = 0; i < duration.length; i++){
      if(start[i] < max && (start[i]+duration[i]) > min){
        res.push(i);
      }
    }
    return res;
  }

  function display() {
    var rects, labels,
        minExtent = brush.extent()[0],
        maxExtent = brush.extent()[1],
        visItems = getVisIndices(minExtent, maxExtent);
    //items.filter(function(d) {return d.start < maxExtent && d.end > minExtent;});

    mini.select(".brush")
      .call(brush.extent([minExtent, maxExtent]));

    x1.domain([minExtent, maxExtent]);
    xMainAxis.ticks(10);

    // update the axis
    main.select('.main.axis.date').call(xMainAxis);

    //update main item rects
    rects = itemRects.selectAll("rect")
      .data(visItems, function(d) { return d; })
      .attr("x", function(d) {return x1(start[d]);})
      .attr("width", function(d) {return x1(duration[d] + start[d]) - x1(start[d]);});

    rects.enter().append("rect")
      .attr("class", "task")
      .attr("id", function(d) {return d; })
      .attr("x", function(d) {return x1(start[d]);})
      .attr("y", function(d) {return y1(loc[d]) + 10;})
      .attr("width", function(d) {return x1(duration[d] + start[d]) - x1(start[d]);})
      .attr("height", function(d) {return .8 * y1(1);})
      .attr("fill", "steelblue");

    rects.exit().remove();

    //update the item labels
    labels = itemRects.selectAll("text")
      .data(visItems, function (d) { return d; })
      .attr("x", function(d) {return x1(Math.max(start[d], minExtent) + 2);});

    labels.enter().append("text")
      .text(function(d) {return kernels[d];})
      .attr("id", function(d){return d;})
      .attr("x", function(d) {return x1(Math.max(start[d], minExtent) + 0.5);})
      .attr("y", function(d) {return y1(loc[d] + .5);})
      .attr("text-anchor", "start")
      .attr("fill", "burlywood")
      .attr("font-weight", "bold");

    labels.exit().remove();
    popoverTasks();
  }

  function popoverTasks(){
    var minExtent = brush.extent()[0];
        maxExtent = brush.extent()[1];
    //popovers for every task
    $(".task").each(function(index, domEle){
        $(domEle).mouseover(function(){
          $(domEle).attr("fill", "#F4A460");
          });

        $(domEle).mouseout(function(){
          $(domEle).attr("fill", "steelblue");
          });

        $(domEle).popoverMod({
          'placement':'bottom', //for getting the arrow
          'title': $(domEle).attr('title', kernels[parseInt($(domEle).attr("id"))]),
          'content':$(domEle).attr('data-content', tooltip[parseInt($(domEle).attr("id"))]),
          'manualPosition': true,
          'position': {
              top:  y1(loc[parseInt($(domEle).attr("id"))] + .5) + 90, //parseInt($(domEle).attr("y"))+90, 
              left: x1(Math.max(start[parseInt($(domEle).attr("id"))], minExtent) + 0.5)+270 //600 //parseInt(x1($(domEle).attr("x")))
            }
            // 90 is a voodoo constant
         });
     });
  }

}


function mouseover(d, i) {
  d3.select(this)
    .attr("fill", "#F4A460");
  //showHide(cssline_in_source[i]);
}

function mouseout(d, i) {
  d3.select(this)
    .attr("fill", "steelblue");
  //showHide(cssline_in_source[i]);
}

function showHide(shID) {
  $("#"+shID).toggleClass("highlighted"); 
}

function totalDuration(durations, starts, locations) {
  var endingTimes = new Array();
  for (var i = 0; i < durations.length; i++) {
    endingTimes[i] = durations[i] + starts[i];
  }
  return Math.max.apply(null, endingTimes);
}

