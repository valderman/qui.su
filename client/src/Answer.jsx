import React from 'react';
import './css/Answer.css';

function Answer(props) {
    const alts = props.alts || [];
    const submit = props.onSubmit ? props.onSubmit : (() => undefined);
    const renderAlt = (key, text) => {
        if(props.preview || props.overhead) {
            const answerClass = props.preview ? "preview" : "overhead";
            return (
                <p
                    className={answerClass}
                    key={key}
                    dangerouslySetInnerHTML={{__html:text}}
                />
            );
        } else {
            return (
                <button
                    onClick={() => submit(key)}
                    key={key}
                    dangerouslySetInnerHTML={{__html:text}}
                />
            );
        }
    };
    return (
        <div className="answer">
            {alts.map(({key, text}) => renderAlt(key, text))}
        </div>
    );
}

export default Answer;
