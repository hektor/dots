#!/usr/bin/env node

const fs = require('fs')

const objLength = obj => Object.keys(obj).length

fs.readFile(process.argv[2], (err, data) => (err ? console.error(err) : console.log(objLength(JSON.parse(data)))))
