const express = require('express');
const app = express();

let words = "random friggin words man you dont know what youre gonna get".split(' ');


app.get('/', (req, res) => {
    let randy = Math.floor(Math.random() * 9);
    let randomWord = words[randy];
    return res.json(randomWord);
});

app.listen(3001, () => console.log('Example app listening on port 3001!'))