import React, {useState, useEffect} from 'react';
import hljs from 'highlight.js';

function QuizEditor(props) {
    const [editing, edit] = useState(props.editing || !props.lockable || false);
    const [quiz, setQuiz] = useState(props.initialValue);
    const saveHandler = async () => {
        const q = document.querySelector('.quizEditor #quizBox');
        (props.onSave || (() => undefined))(q.innerText || q.value);
    };
    useEffect(() => {
        const block = document.querySelector('.quizEditor pre code');
        if(block) {
            hljs.highlightBlock(block);
        }
        if(editing && props.focus) {
            document.querySelector('#quizBox').focus();
        }
    }, [editing]);
    useEffect(() => {
        setQuiz(props.initialValue);
    }, [props.initialValue]);

    return (
        <div className="quizEditor">
        {editing
            ? <textarea
                id="quizBox"
                value={quiz}
                onChange={e => setQuiz(e.target.value)}
                rows={Math.min(25, quiz.split('\n').length)}
            />
            : <pre>
                <code
                      id="quizBox"
                      className="language-markdown"
                  >
                      {quiz}
                  </code>
            </pre>
        }
        <button onClick={saveHandler}>{props.saveText || "Save"}</button>
        {props.lockable && (editing
        ? <button onClick={() => edit(false)}>Stop editing</button>
        : <button onClick={() => edit(true)}>Edit this quiz</button>)
        }
        </div>
    );
}

export default QuizEditor;