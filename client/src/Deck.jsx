import React from 'react';
import Question from './Question.jsx';
import QuestionPieChart from './QuestionPieChart.jsx';
import './css/Deck.css';

const SIT_TIGHT = "Sit tight, the next question is coming right up!";
const THE_END = "Be patient, this quiz hasn't started yet!";

class Deck extends React.Component {
    constructor(props) {
        super(props);
        if(!props.id && !props.url) {
            throw 'either id or url is mandatory';
        }
        this.api = props.api;
        this.state = {
            waiting: true,
            done: false,
            quizId: props.id,
            url: props.url,
            stats: false,
            answeredQuestions: []
        };
        this.overheadMode = !!this.props.overhead;
    }

    async setNextQuestion(q) {
        let state = {
            waiting: this.alreadyAnswered(q.key),
            done: q.done
        };
        if(q.done) {
            state.text = '';
            state.alts = [];
            state.id = -1;
        } else {
            state.text = q.text;
            state.alts = q.alts;
            state.id = q.key;
        }
        state.stats = q.stats;
        this.setState(state);
        const nextQ = await this.api.waitForPush(this.state.quizId);
        this.setNextQuestion(nextQ);
    }

    async componentDidMount() {
        const then = async () => {
            const q = await this.api.getQuestion(this.state.quizId);
            this.setNextQuestion(q);
        };
        if(!this.state.quizId) {
            const qid = await this.api.resolveQuiz(this.state.url);
            this.setState({quizId: qid}, then);
        } else {
            const url = await this.api.getQuizUrl(this.state.quizId);
            this.setState({url: url}, then);
        }
    }

    async submit(key) {
        await this.api.answer(this.state.quizId, key);
        const state = {
            waiting: true,
            answeredQuestions: this.state.answeredQuestions.concat(this.state.id)
        };
        this.setState(state);
    }

    alreadyAnswered(qid) {
        return (this.state.answeredQuestions.indexOf(qid) >= 0);
    }

    renderContent() {
        if(this.state.stats) {
            return (
                <QuestionPieChart
                    question={this.state.text}
                    alts={this.state.alts}
                />
            )
        } else if(this.state.waiting) {
            return (<p className="message">{SIT_TIGHT}</p>);
        } else if(this.state.done) {
            return (<p className="message">{THE_END}</p>);
        } else {
            return (
                <Question
                    text={this.state.text}
                    alts={this.state.alts}
                    overhead={this.overheadMode}
                    onSubmit={this.submit.bind(this)}
                    url={this.state.url}
                />
            );
        }
    }

    render() {
        const protocol = window.location.protocol;
        const participant = window.location.host + '/' + this.state.url;
        const partLink = protocol + '//' + participant;
        return (
            <div className="deckContainer">
            <div className="deck">{this.renderContent()}
                {this.overheadMode &&
                    <p className="participationLink">
                        To participate, go to <a href={partLink}>{participant}</a>
                    </p>
                }
            </div>
            </div>
        );
    }
}

export default Deck;
