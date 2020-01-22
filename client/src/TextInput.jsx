import React, { useState } from 'react';

const STATE_IDLE = 0;
const STATE_BUFFERING = 1;

function TextInput(props) {
    const val = typeof props.value !== 'undefined' ? props.value : null;
    const throttle = typeof props.throttle !== 'undefined'
        ? props.throttle
        : 500;
    const [value, setValue] = useState(val === null ? '' : val);
    const [state, setState] = useState(STATE_IDLE);
    const [count, setCount] = useState(0);
    const [changeEvent, setChangeEvent] = useState(null);
    const onChange = e => {
        setValue(val === null ? e.target.value : val);
        e.persist();
        setChangeEvent(e);
        setCount(count+1);
    };

    if(state === STATE_IDLE && count > 0) {
        changeEvent.count = count;
        props.onChange.bind(null)(changeEvent);
        setCount(0);
        setState(STATE_BUFFERING);
        window.setTimeout(() => setState(STATE_IDLE), throttle);
    }
    return (
        <input
            type="text"
            value={value}
            placeholder={props.placeholder}
            onChange={onChange}
            onKeyUp={props.onKeyUp}
            onKeyDown={props.onKeyDown}
            onFocus={props.onFocus}
            onBlur={props.onBlur}
            onClick={props.onClick}
            onMouseOver={props.onMoseOver}
            onMouseOut={props.onMoseOut}
            onMouseMove={props.onMoseMove}
        />
    );
}

export default TextInput;
