var data = $.getJSON('datafiles/sniperRatioVictory.json', function(test_data){
    nv.addGraph({
        generate: function() {
            /*var width = nv.utils.windowSize().width,
                height = nv.utils.windowSize().height;*/
            var width = 700,
                height = 600;
            
            var chart = nv.models.multiBarChart()
                .width(width)
                .height(height)
                .stacked(true)
                ;

            chart.dispatch.on('renderEnd', function(){
                console.log('Render Complete');
            });

            var svg = d3.select('#sniperRatioVictoryPlot svg').datum(test_data);
            console.log('calling chart');
            svg.transition().duration(0).call(chart);

            chart.yAxis
        		.tickFormat(d3.format(',.3f'));

            chart.width(width);
            chart.height(height);
            return chart;
        },
        callback: function(graph) {
            nv.utils.windowResize(function() {
                var width = 700,
                height = 600;
                graph.width(width).height(height);

                d3.select('#sniperRatioVictoryPlot svg')
                    .attr('width', width)
                    .attr('height', height)
                    .transition().duration(0)
                    .call(graph);

            });
        }
    });
});