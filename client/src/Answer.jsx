import React from 'react';
import './css/Answer.css';

function Answer(props) {
    const alts = props.alts || [];
    const submit = key => props.onSubmit ? props.onSubmit(key) : null;
    const renderAlt = (key, text) => {
        if(props.preview) {
            return (
                <p
                    className="preview"
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
