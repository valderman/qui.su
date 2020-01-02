import React from 'react';
import { Redirect } from 'react-router-dom';
import Uploader from './Uploader.jsx';
import List from './List.jsx';
import "./css/QuizList.css";

class QuizList extends React.Component {
    constructor(props) {
        super(props);
        this.api = props.api;
        this.onSelect = (props.onSelect || (() => undefined)).bind(null);
        this.state = { quizzes: [], selected: null, goto: null };
        this.buttons = [
            {
                title: 'Start or resume quiz',
                className: 'fa-play-circle',
                onClick: q => this.setState({goto: '/present/' + q.key})
            },
            {
                title: 'Open overhead view (new window)',
                className: 'fa-chalkboard-teacher',
                onClick: q => window.open('/overhead/' + q.key)
            },
            {
                title: 'Erase all quiz answers',
                className: 'fa-eraser',
                onClick: this.resetAnswers.bind(this)
            },
            {
                title: 'Delete quiz',
                className: 'fa-trash-alt',
                onClick: this.deleteQuiz.bind(this)
            },
        ];
    }

    async componentDidMount() {
        await this.fetchQuizzes();
    }

    async fetchQuizzes() {
        const quizzes = await this.api.getQuizzes();
        this.setState({ quizzes: quizzes.map(q => ({...q, key: q.quizId})) });
    }

    async onUpload(q) {
        await this.fetchQuizzes();
    }

    async resetAnswers(quiz) {
        const msg = "Reset answers for " + quiz.name + "? This cannot be undone.";
        if(window.confirm(msg)) {
            await this.api.resetAnswers(quiz.key);
        }
    }

    async deleteQuiz(q) {
        const msg = "Really delete " + q.name + "? This cannot be undone.";
        if(window.confirm(msg)) {
            if(this.state.selected && this.state.selected.key === q.key) {
                this.select(null);
            }
            await this.api.deleteQuiz(q.key);
            await this.fetchQuizzes();
        }
    }

    render() {
        if(this.state.goto) {
            return (<Redirect to={this.state.goto} />)
        }
        return (
            <div className="quizList">
                <List
                    onSelect={q => this.onSelect(q)}
                    buttons={this.buttons}
                    items={this.state.quizzes}
                />
                <Uploader
                    api={this.props.api}
                    onUpload={q => this.onUpload(q)}
                />
            </div>
        );
    }
}

export default QuizList;
