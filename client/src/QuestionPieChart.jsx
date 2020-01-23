import React    from 'react';
import PieChart from 'react-minimal-pie-chart';
import './css/PieChart.css';

const COLORS = [
    '#118f00',
    '#9fa725',
    '#f3bd6a',
    '#ea8050',
    '#d43d51'
];

function stripHTML(s) {
    const doc = new DOMParser().parseFromString(s, 'text/html');
    return doc.body.textContent || "";
}

function shorten(s) {
    return s.split('\n')[0];
}

function fromAlt(alt, ix) {
    const text = (alt.correct ? '✓' : '') + stripHTML(alt.text);
    return {
        title: text.length > 16
             ? text.substring(0,13) + '...'
             : text,
        value: alt.responses,
        color: COLORS[ix % COLORS.length]
    };
}

function filterZeroes(alts) {
    return alts.filter(a => a.responses > 0);
}

function QuestionPieChart(props) {
    if(props.alts.every(a => a.responses === 0)) {
        return (<h2>Nobody responded to this question yet. :(</h2>);
    } else {
        return (
            <div className="pieChartContainer">
                <h2>
                    {shorten(stripHTML(props.question))}
                </h2>
                <PieChart
                    className="pieChart"
                    data={filterZeroes(props.alts).map(fromAlt)}
                    label={p => {
                            const title = p.data[p.dataIndex].title;
                            const perc = Math.round(p.data[p.dataIndex].percentage);
                            return title + ' (' + perc + '%)'
                    }}
                    radius={20}
                    labelPosition={110}
                    viewBoxSize={[100,50]}
                    labelStyle={{
                        fill: 'black',
                        fontSize: '0.15em',
                        fontFamily: 'sans-serif'
                    }}
                />
            </div>
        );
    }
}

export default QuestionPieChart;
