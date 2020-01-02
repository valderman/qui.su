import React, { useState } from 'react';
import "./css/fileUpload.css";

const WAITING = 0;
const UPLOADING = 1;
const ERRORED = 2;

const SUPPORTED_TYPES = [
    "text/markdown"
];

function preventDefault(e) {
    e.preventDefault();
    e.stopPropagation();
}

function Uploader(props) {
    const [state, setState] = useState(WAITING);
    const [error, setError] = useState(null);

    const handleUpload = async (f) => {
        if(!SUPPORTED_TYPES.some(t => t === f.type)) {
            setState(ERRORED);
            setError("The given file type is not supported.");
            return;
        }

        setState(UPLOADING);
        try {
            const qid = await props.api.newQuiz(f);
            setState(WAITING);
            const fun = props.onUpload;
            fun(qid);
        } catch (e) {
            setState(ERRORED);
            setError("Are you sure it is a valid Markdown file?");
        }
    };

    const uploadForm = () => (
        <form
            className="fileUpload"
            onDragOver={preventDefault}
            onDragEnter={preventDefault}
            onDrop={e => {
                    preventDefault(e);
                    handleUpload(e.dataTransfer.files[0]);
            }}
        >
            <input
                type="file"
                accept=".md"
                id="fileUpload"
                onChange={e => handleUpload(e.target.files[0])}
            />
            <label htmlFor="fileUpload">
                <p>Drag a quiz file (.md) here to upload!</p>
                <p className="pick">(Or click me to pick a file)</p>
            </label>
        </form>
    );

    switch(state) {
    case WAITING:
        return uploadForm();
    case UPLOADING:
        return (
            <div className="fileUpload">
                Uploading file...
            </div>
        );
    case ERRORED:
        return (
            <div className="fileUpload">
                {uploadForm()}
                <p className="error">
                    We couldn't process your quiz.
                    {error}
                </p>
            </div>
        );
    default:
    }
}

export default Uploader;
