/* global exports */
"use strict";

// module Thermite

exports.createClassImpl = function(runAction) {
    return function(maybe) {
        return function(spec) {
            return React.createClass({
                getInitialState: function() {
                    return {
                        value: spec.initialState
                    };
                },
                performAction: function(action) {
                    runAction(this)(spec.performAction(this.props)(action))();
                },
                render: function() {
                    var children = Array.isArray(this.props.children) ? this.props.children : [this.props.children];
                    return spec.render(this)(this.state.value)(this.props)(children);
                },
                componentWillMount: function() {
                    var self = this;
                    maybe(function() {})(function(action) {
                        return function() {
                            self.performAction(action);Ó
                        };
                    })(spec.componentWillMount)();
                },
                displayName: maybe(undefined)(function(a) {
                    return a;
                })(spec.displayName)
            })
        };
    };
};