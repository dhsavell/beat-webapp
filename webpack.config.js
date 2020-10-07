const PrettierPlugin = require("prettier-webpack-plugin");
const CopyWebpackPlugin = require("copy-webpack-plugin");
const webpack = require("webpack");

module.exports = (env, argv) => ({
    module: {
        rules: [{
            test: /\.html$/,
            exclude: /node_modules/,
            loader: 'file-loader?name=[name].[ext]'
        }, {
            test: /\.elm$/,
            exclude: [/elm-stuff/, /node_modules/],
            use: {
                loader: 'elm-webpack-loader',
                options: {
                    optimize: argv.mode === 'production'
                }
            }
        }, {
            test:  /\.s[ac]ss$/i,
            use: ['style-loader', 'css-loader', 'sass-loader'],
        }]
    },

    devServer: {
        inline: true,
        stats: 'errors-only'
    },

    plugins: [
        new PrettierPlugin(),
        new webpack.EnvironmentPlugin(["BASE_URL", "VERSION"]),
        new CopyWebpackPlugin([
            { from: 'public' }
        ])
    ]
});
