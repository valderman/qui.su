import React, { useState, useEffect } from 'react';
import { useHistory } from 'react-router-dom';
import Uploader from './Uploader.jsx';
import List from './List.jsx';
import "./css/QuizList.css";

function QuizList(props) {
    const [quizzes, setQuizzes] = useState([]);
    const [selected, select] = useState(null);
    const history = useHistory();

    const onSelect = e => {
        select(e);
        (props.onSelect || (() => undefined))(e);
    };

    const fetchQuizzes = async () => {
        const qs = await props.api.getQuizzes();
        setQuizzes(qs.map(q => ({...q, key: q.quizId})));
    };
    useEffect(() => {
        fetchQuizzes();
    }, [props.tick]);

    const resetAnswers = async (quiz) => {
        const msg = "Reset answers for " + quiz.name + "? This cannot be undone.";
        if(window.confirm(msg)) {
            await props.api.resetAnswers(quiz.key);
        }
    };

    const deleteQuiz = async (q) => {
        const msg = "Really delete " + q.name + "? This cannot be undone.";
        if(window.confirm(msg)) {
            if(selected && selected.key === q.key) {
                onSelect(null);
            }
            await props.api.deleteQuiz(q.key);
            await fetchQuizzes();
        }
    };

    const buttons = [
        {
            title: 'Start or resume quiz',
            className: 'fa-play-circle',
            onClick: q => history.push('/present/' + q.key)
        },
        {
            title: 'Open overhead view (new window)',
            className: 'fa-chalkboard-teacher',
            onClick: q => window.open('/overhead/' + q.key)
        },
        {
            title: 'Erase all quiz answers',
            className: 'fa-eraser',
            onClick: resetAnswers
        },
        {
            title: 'Delete quiz',
            className: 'fa-trash-alt',
            onClick: deleteQuiz
        },
    ];

    return (
        <div className="quizList">
            <Uploader
                api={props.api}
                onUpload={fetchQuizzes}
            />
            <List
                onSelect={onSelect}
                buttons={buttons}
                items={quizzes}
            />
        </div>
    );
}

export default QuizList;
