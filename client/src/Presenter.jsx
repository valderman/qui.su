import React from 'react';
import Question from './Question.jsx';
import QuestionPieChart from './QuestionPieChart.jsx';
import './css/Deck.css';

class Presenter extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            alts: [],
            qix: 0,
            qids: [],
            stats: false,
            id: props.id
        };
        this.api = props.api;
        this.preview = props.preview || false;
        this.keyPress = this.keyPress.bind(this);
    }

    async componentDidMount() {
        const p = this.api.getQuestionIds(this.state.id);
        let q, qids;
        if(this.preview) {
            qids = await p;
            q = await this.api.getQuestionById(qids[0]);
        } else {
            q = await this.api.getQuestion(this.state.id);
            qids = await p;
        }
        const url = await this.api.getQuizUrl(this.state.id);
        this.setState({qids: qids, url: url}, () => {
            this.updateQuestion(q);
        });
        document.addEventListener('keyup', this.keyPress);
    }

    componentWillUnmount() {
        document.removeEventListener('keyup', this.keyPress);
    }

    updateQuestion(q) {
        let qix = -1;
        for(let i in this.state.qids) {
            if(this.state.qids[i] === q.key) {
                qix = Number(i);
                break;
            }
        }

        let state = {qix: qix};
        state.alts = q.done ? [] : q.alts;
        state.stats = q.stats;
        state.text = q.text || 'Your quiz is all set up and good to go!';
        this.setState(state);
    }

    async step(n) {
        const next = this.state.qix + n === -2
                   ? this.state.qids.length - 1
                   : this.state.qix + n;
        if(next < 0) {
            return;
        } else if(!this.state.done && next >= this.state.qids.length) {
            if(!this.preview) {
                await this.api.finishQuiz(this.state.id);
            }
            this.updateQuestion({
                done: true,
                text: "End of quiz, go forward to restart!"
            });
        } else {
            const nextId = this.state.qids[next];
            let q;
            if(this.preview) {
                q = await this.api.getQuestionById(nextId);
            } else {
                q = await this.api.setQuestion(this.state.id, nextId);
            }
            this.updateQuestion(q);
        }
    }

    async toggleStats() {
        if(!this.hasQuestion()) {
            return;
        }
        const qid = this.state.qids[this.state.qix];
        let q;
        if(!this.preview) {
            q = await this.api.showStats(this.state.id, qid, !this.state.stats);
        } else {
            q = await this.api.getQuestionById(qid);
            q.stats = !this.state.stats;
        }
        this.updateQuestion(q);
    }

    hasQuestion() {
        return this.state.qix >= 0;
    }

    keyPress(e) {
        switch(e.keyCode) {
            case 37:
                this.step(-1);
                break;
            case 39:
                this.step(1);
                break;
            case 66:
                this.toggleStats();
                break;
            default:
        }
    }

    questionOrStats() {
        if(this.state.stats) {
            return (
                <QuestionPieChart
                    question={this.state.text}
                    alts={this.state.alts}
                />
            );
        } else {
            return (
                <Question
                    text={this.state.text}
                    alts={this.state.alts}
                    overhead={true}
                    preview={this.preview}
                    url={this.state.url}
                />
            );
        }

    }

    render() {
        const protocol = window.location.protocol;
        const participant = window.location.host + '/' + this.state.url;
        const partLink = protocol + '//' + participant;
        const textClass = this.preview ? "deck preview" : "deck";
        return (
            <div className={textClass} id="presenter">
                {this.questionOrStats()}
                {this.preview ||
                    <p className="participationLink">
                        To participate, go to <a href={partLink}>{participant}</a>
                    </p>
                }
                <button onClick={() => this.step(-1)}>Previous</button>
                {this.hasQuestion() &&
                 <button onClick={() => this.toggleStats()}>
                     {"Show " + (this.state.stats ? "question" : "stats")}
                 </button>
                }
                <button onClick={() => this.step(1)}>Next</button>
                <p>
                    {this.state.qix >= 0
                     ? (this.state.qix+1) + " / " + this.state.qids.length
                     : "Quiz done!"
                    }
                </p>
            </div>
        );
    }
}

export default Presenter;
