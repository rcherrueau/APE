//  ,---.    ____      __       ___          __   __      ,---.
// ( @ @ )  / __ \____/ /____  / _ \___ ____/ /__/ /_ __ ( @ @ )
//  ).-.(  / /_/ / __/ __/ _ \/ // / _ `/ _  / _  / // /  ).-.(
// '/|||\` \____/\__/\__/\___/____/\_,_/\_,_/\_,_/\_, /  '/|||\`
//   '|`                                         /___/     '|`
//
// Adapted Calculator for my Daddy's Needs

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
    ],
    output: {
        filename: 'www/app.js',
    },
    module: {
        rules: [
            // Sass part
            {
                test: /\.scss$/,
                use: [
                    {
                        loader: 'file-loader',
                        options: {
                            name: 'www/app.css',
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
                options: {
                    verbose: true,
                    debug: true
                }
            }
        ]
    },
}];
