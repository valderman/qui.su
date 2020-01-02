import React, { useEffect } from 'react';
import hljs    from 'highlight.js';
import Answer  from './Answer.jsx';
import './css/Question.css';

function Question(props) {
    useEffect(() => {
        document.querySelectorAll('.question pre code').forEach(block => {
            hljs.highlightBlock(block);
        });
    });
    let textClass = "";
    if(props.overhead) {
        textClass += "overhead";
    }
    if(props.preview) {
        textClass += " preview";
    }
    return (
        <div className={"question " + textClass}>
            <div
                className={textClass}
                dangerouslySetInnerHTML={{__html:props.text}}
            />
            {(props.overhead && !props.preview) ||
                <Answer
                    alts={props.alts}
                    onSubmit={props.onSubmit}
                    preview={props.preview}
                />
            }
        </div>
    );
}

export default Question;
