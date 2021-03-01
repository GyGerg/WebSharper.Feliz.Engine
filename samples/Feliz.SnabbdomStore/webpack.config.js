// @ts-check
const path = require('path');

const mode = process.env.NODE_ENV || 'development';
const prod = mode === 'production';
console.log(`Bundling for ${mode}...`)

module.exports = {
	entry: {
		bundle: ['./App.fs.js']
	},
	output: {
		path: __dirname + '/public',
		filename: '[name].js',
		chunkFilename: '[name].[id].js',
	},
	mode,
	devtool: prod ? false: 'source-map'
};
