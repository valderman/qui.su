import React, { useState } from 'react';
import "./css/List.css";

function List(props) {
    const onSelect = (props.onSelect || (() => undefined));
    const [selected, setSelected] = useState(null);

    const toggleSelect = (e, item) => {
        e.target.blur();
        if(isSelected(item.key)) {
            item = null;
        }
        setSelected(item);
        onSelect(item);
    };

    const isSelected = key => selected && selected.key === key;

    const renderButton = (b, i) =>
        <button
            className={'fas ' + b.className}
            title={b.title instanceof Function ? b.title(i) : b.title}
            onClick={e => {
                b.onClick(props.items[i]);
                e.target.blur();
            }}
            key={b.key || b.title || b.className}
        />;

    const renderItem = (i, ix) =>
        <li key={i.key}>
            <button
                className="selectButton"
                onClick={e => toggleSelect(e, i)}
            >
                {isSelected(i.key) && <span className="fas fa-check" />}
                {i.name}
            </button>
            <div className="buttons">
                {props.buttons.map(b => renderButton(b, ix))}
            </div>
        </li>;

    return (<div className="itemList">{props.items.map(renderItem)}</div>);
}

export default List;
