
const idx = stats[0].reduce(
    (acc, val, i) => acc.set(val, i),
    new Map()
)
const header = stats[0];
const rows = stats.slice(1);

class Stats {
    constructor(keys, values) {
        for (const i in header) {
            this[keys[i]] = values[i];
        }
    }
    get date() {
        return Date.parse(this.year + '-12-' + this.day);
    }
    get dateLabel() {
        return this.year + '-' + this.day;
    }
}

const data = rows.map(function (value, i, arr) {
    return new Stats(header, rows[i]);
}).toSorted(function (a, b) {
    return a.date - b.date;
});

let minMaxBy = function (f, data) {
    const xs = data.map(f);
    return [Math.min(...xs), Math.max(...xs)];
}

const bounds = new Map();
for (var i = 0; i < header.length; i++) {
    bounds.set(header[i], minMaxBy(e => e[i], rows));
}

let formatSizeBytes = function (bytes) {
    if (bytes === 0) {
        return "0.00 B";
    }

    let e = Math.floor(Math.log(bytes) / Math.log(1024));
    return (bytes / Math.pow(1024, e)).toFixed(2) +
        ' ' + ' KMGTP'.charAt(e) + 'B';
}

const charts = [];

const skippedArea = {
    id: 'skippedArea',
    beforeDatasetsDraw(chart, args, pluginOptions) {
        const { ctx, data, chartArea: { left, right, top, bottom, width, height }, scales: { x, y } } = chart;
        const tickWidth = width / x.max - 1;

        ctx.save();

        for (var i = 0; i < data.datasets[0].data.length;) {
            const p1 = data.datasets[0].data[i];
            var j = i + 1;

            if (p1 !== null) {
                i = j;
                continue;
            }
            while (j < data.datasets[0].data.length && data.datasets[0].data[j] === null) {
                j++;
            }
            const p2 = data.datasets[0].data[j];
            ctx.fillStyle = 'rgba(0,0,0,0.1)';
            ctx.fillRect(x.getPixelForValue(i) - tickWidth / 2, top, x.getPixelForValue(j) - x.getPixelForValue(i), height);
            ctx.restore();
            i = j;
        }
    }
}


const datasets = [
    { label: 'total_wall_seconds' },
    { label: 'total_cpu_seconds', hidden: true },
    { label: 'allocated_bytes' },
    { label: 'average_bytes_used' },
    { label: 'max_bytes_used' }
].map(function name(ds, index, array) {
    switch (true) {
        case ds.label.endsWith('_seconds'):
            yAxisID = 'y';
            break;
        case ds.label.endsWith('_bytes'):
        case ds.label.endsWith('_bytes_used'):
            yAxisID = 'y1';
            break;
        default:
            yAxisID = 'y2';
            break;
    }
    return {
        parsing: { yAxisKey: ds.label },
        yAxisID: yAxisID,
        spanGaps: true,
        ...ds,
    }
});

const adjustAxes = {
    id: 'adjustAxes',
    beforeInit(chart, args, pluginOptions) {
        const visible = datasets.map(function (ds) {
            return !ds.hidden;
        })
        adjAxes(chart, visible);
    }
}

Chart.register(skippedArea);
Chart.register(adjustAxes);

function adjAxes(chart, visible) {
    var yMax = 0;
    var y1Max = 0;
    for (const i in datasets) {
        const ds = datasets[i];
        if (!visible[i]) {
            continue;
        }
        const dsHi = bounds.get(ds.label)[1];
        if (ds.yAxisID == 'y') {
            yMax = Math.max(yMax, dsHi);
        } else if (ds.yAxisID == 'y1') {
            y1Max = Math.max(y1Max, dsHi);
        }
    }

    // recalculate suggestedMax
    for (i in charts) {
        const ch = charts[i];
        if (yMax > 0) {
            ch.config.options.scales.y.suggestedMax = yMax;
            ch.update();
        }
        if (y1Max > 0) {
            ch.config.options.scales.y1.suggestedMax = y1Max;
            ch.update();
        }
    }
}

for (var year = bounds.get('year')[1]; year >= bounds.get('year')[0]; year--) {
    const yearlyData = data.filter((d) => { return d.year == year });
    const yearlyDatasets = datasets.map((ds) => {
        return { data: yearlyData, ...ds };
    });

    const divid = 'chart-' + year;  // FIXME: (Ab)used in onClick!
    const $div = $(`<div style='position: relative; margin-bottom=20px; height:80vh; width:80vw'><canvas id='${divid}' class='chart'></canvas></div>`).appendTo('body');
    const ctx = document.getElementById(divid);

    let chart = new Chart(ctx, {
        type: 'bar',
        data: {
            labels: yearlyData.map(function (val, i, arr) {
                return val.dateLabel;
            }),
            datasets: yearlyDatasets,
        },
        options: {
            parsing: {
                xAxisKey: 'dateLabel',
            },
            events: ['click', 'touchstart', 'touchmove', 'mousemove', 'mouseout'],
            plugins: {
                title: {
                    display: true,
                    text: year,
                },
                tooltip: {
                    position: 'nearest',
                    callbacks: {
                        title: function (ctx) {
                            const entry = yearlyData[ctx[0].dataIndex];
                            return `day ${entry.day}: ${entry.name}`;
                        },
                    },
                },
                legend: {
                    onClick: function (event, legendItem, legend) {
                        Chart.defaults.plugins.legend.onClick(event, legendItem, legend);

                        const visible = [];
                        for (i in event.chart.data.datasets) {
                            visible.push(chart.getDatasetMeta(i).visible);
                        }

                        adjAxes(event.chart, visible);

                        // show or hide dataset
                        for (i in charts) {
                            const ch = charts[i];
                            if (ch === chart) {
                                continue;
                            }

                            if (legendItem.hidden) {
                                ch.hide(legendItem.datasetIndex);
                            } else {
                                ch.show(legendItem.datasetIndex);
                            }
                        }
                    },
                },
            },
            interaction: {
                mode: 'index',
                intersect: false,
            },
            elements: {
                bar: {
                    borderWidth: 2,
                }
            },
            responsive: true,
            scaleShowValues: true,
            scales: {
                x: {
                    ticks: {
                        autoSkip: false,
                    },
                },
                y: {
                    type: 'linear',
                    display: true,
                    position: 'left',
                    beginAtZero: true,
                    ticks: {
                        callback: function (val, index) {
                            return val + " s";
                        },
                    },
                },
                y1: {
                    type: 'linear',
                    display: true,
                    position: 'right',
                    beginAtZero: true,
                    // grid line settings
                    grid: {
                        drawOnChartArea: false, // only want the grid lines for one axis to show up
                    },
                    ticks: {
                        callback: function (val, index) {
                            return formatSizeBytes(val);
                        },
                    },
                },
            },
            onClick: (e) => {
                const chart = e.chart;
                const canvasPosition = Chart.helpers.getRelativePosition(e, chart);
                const dataX = chart.scales.x.getValueForPixel(canvasPosition.x);
                const day = "" + (dataX + 1);
                const year = chart.canvas.id.slice(6);
                const proj = "aoc" + year + "-" + day.padStart(2, '0');
                const URL = `../${proj}.eventlog.html`;
                window.open(URL, '_blank');
                window.open(`../${proj}.prof.svg`, '_blank');
            },
        },
    });
    charts.push(chart);
}