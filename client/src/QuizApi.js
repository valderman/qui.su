import RestApi from "./RestApi.js";

function parseAlternative(a) {
    return {
        key: a.altId,
        text: a.text,
        responses: a.responses,
        correct: a.correct
    };
}

function parseQuestion(q) {
    return {
        text: q.contents.question.text,
        key: q.contents.question.questionId,
        alts: q.contents.alternatives.map(parseAlternative)
    };
}

function parseNextQuestion(q) {
    if(q.tag === 'Done') {
        return { done: true, text: q.contents };
    }
    let question = parseQuestion(q);
    question.done = false;
    question.stats = q.tag === 'Stats';
    return question;
}

class QuizApi extends RestApi {
    constructor(baseUrl, storageManager, onExpire) {
        super(baseUrl || "", storageManager, onExpire);
    }

    async answer(quizId, altId) {
        const body = altId;
        return await this.post(["answer", quizId], body);
    }

    async waitForPush(quizId) {
        const q = await this.longPoll(["wait", quizId]);
        return parseNextQuestion(q);
    }

    async waitForAnswer(quizId) {
        return await this.longPoll(["answers", quizId]);
    }

    async getQuestion(quizId) {
        const q = await this.get(["quiz", quizId]);
        return parseNextQuestion(q);
    }

    async getQuestionById(questionId) {
        const q = await this.get(["question", questionId]);
        return parseQuestion({contents: q});
    }

    async setQuestion(quizId, questionId) {
        const q = await this.get(["notify", quizId, questionId, false]);
        return parseNextQuestion(q);
    }

    async showStats(quizId, questionId, stats) {
        const q = await this.get(["notify", quizId, questionId, stats]);
        return parseNextQuestion(q);
    }

    async finishQuiz(quizId) {
        return await this.get(["end", quizId]);
    }

    async getQuestionIds(quizId) {
        return await this.get(["questions", quizId]);
    }

    async newQuiz(quiz) {
        return await this.post(["quiz"], quiz, 'application/octet-stream');
    }

    async getQuizzes() {
        return (await this.get(["quizzes"]));
    }

    async deleteQuiz(quizId) {
        return await this.get(["delete", quizId]);
    }

    async resetAnswers(quizId) {
        return await this.get(["reset", quizId]);
    }

    async resolveQuiz(quizUrl) {
        return await this.get(["resolve", quizUrl]);
    }

    async getQuizUrl(quizId) {
        return await this.get(["url", quizId]);
    }

    async authenticate(token) {
        const result = await this.post(["auth"], token);
        this.setAuthToken(result.token, result.expires);
        return result.user;
    }

    async findUsers(search, max) {
        return await this.get(["users", encodeURI(search || '%'), max]);
    }
}

export default QuizApi;
