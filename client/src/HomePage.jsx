import React, { useState } from 'react';
import QuizList            from './QuizList.jsx';
import Presenter           from './Presenter.jsx';
import MarkdownTutorial    from './MarkdownTutorial.jsx';
import './css/HomePage.css';

function HomePage(props) {
    const [selectedQuiz, selectQuiz] = useState(undefined);
    const [tick, setTick] = useState(0);
    return (
        <main className="homePage">
            <h1>Congratulations, you're logged in!</h1>
            <h3>&mdash; Uh, great, so what do I do now?</h3>
            <p>
                Great question! We're planning to add a bunch of features to
                make this app more fun-friendly (think music quizzes,
                pub quizzes, etc.), but for now it's mainly useful
                for adding interactive quizzes to your lectures.
            </p>
            <div className="widgets">
                <div>
                    <h3>Your quizzes</h3>
                    <QuizList
                        api={props.api}
                        onSelect={selectQuiz}
                        tick={tick}
                    />
                </div>
                {!selectedQuiz &&
                    <MarkdownTutorial
                        api={props.api}
                        onAddQuiz={() => setTick(tick+1)}
                    />
                }
                {selectedQuiz &&
                    <div>
                        <h3>Quiz preview: {selectedQuiz.name}</h3>
                        <Presenter
                            api={props.api}
                            preview={true}
                            id={selectedQuiz.quizId}
                            key={selectedQuiz.quizId}
                        />
                    </div>
                }
            </div>
        </main>
    );
}

export default HomePage;
