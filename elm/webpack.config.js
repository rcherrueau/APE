//  ,---.    ____      __       ___          __   __
// ( @ @ )  / __ \____/ /____  / _ \___ ____/ /__/ /_ __
//  ).-.(  / /_/ / __/ __/ _ \/ // / _ `/ _  / _  / // /
// '/|||\` \____/\__/\__/\___/____/\_,_/\_,_/\_,_/\_, /
//   '|`                                         /___/
//
// Adapted Calculator for my Daddy's Needs

const path = require('path');

// Determine build env
const dev = 'development';
const prod = 'production';
const TARGET_ENV = process.env.npm_lifecycle_event === 'build' ? prod : dev;

// Specific elm make options
const elmOpts = TARGET_ENV === dev
      // Dev
      ? { verbose: true, debug: true }
      // Prod
      : { optimize: true };

// Package application
module.exports = [{
    entry: [
        // Elm MDC specific javascript (e.g., MDC Dialog js -- output
        // into www/app.js)
        './elm-mdc/src/elm-mdc.js',
        // Sass of the app (include MDC css -- output to www/app.css)
        './OctoDaddy.scss',
        // Loading of my Elm OctoDaddy App (output tangles with
        // www/app.js)
        './OctoDaddy.js',
        // Loading of the Index (output in www/index.html)
        './OctoDaddy.html', './favicon.ico'
    ],
    output: {
        path: path.resolve(__dirname, 'www'), // output directory of the build
        filename: 'app.js',
    },
    module: {
        rules: [
            // Index
            {
                test: /OctoDaddy.html$/,
                use: {
                    loader: "file-loader",
                    options: {
                        name: 'index.html',
                    },
                }
            },
            // Sass part
            {
                test: /\.scss$/,
                use: [
                    {
                        loader: 'file-loader',
                        options: {
                            name: 'app.css',
                        },
                    },
                    { loader: 'extract-loader' },
                    { loader: 'css-loader' },
                    { loader: 'sass-loader',
                      options: {
                          includePaths: ['./node_modules']
                      }
                    },
                ]
            },
            // JS part
            {
                test: /\.js$/,
                loader: 'babel-loader',
                query: { presets: ['es2015'] }
            },
            // Elm part (Doesn't include elm-mdc/, this is managed by
            // OctoDaddy package.json)
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/, /elm-mdc/],
                loader: 'elm-webpack-loader',
                options: elmOpts,
            },
            // Assets (including font, favicon)
            // Note: put this in an assets directory?
            { test: /\.(png|ico|ttf|eot|svg|woff(2)?)$/,
              loader: "file-loader",
              options: {
                  name: '[name].[ext]',
              },
            },
        ]
    },
}];
