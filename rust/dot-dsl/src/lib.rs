pub mod graph {
    use graph_items::{edge::Edge, node::Node};

    pub mod graph_items {
        pub mod edge {
            #[attrs::attrs]
            #[derive(Debug, Clone, PartialEq, Eq, Default)]
            pub struct Edge {
                from: String,
                to: String,
            }

            impl Edge {
                pub fn new(from: &str, to: &str) -> Self {
                    Self {
                        from: from.to_string(),
                        to: to.to_string(),
                        ..Self::default()
                    }
                }
            }
        }

        pub mod node {
            #[attrs::attrs]
            #[derive(Debug, Clone, PartialEq, Eq, Default)]
            pub struct Node {
                pub name: String,
            }

            impl Node {
                pub fn new(name: &str) -> Self {
                    Self {
                        name: name.to_string(),
                        ..Self::default()
                    }
                }

                pub fn get_attr(&self, attr: &str) -> Option<&str> {
                    self.attrs.get(attr).map(|x| x.as_str())
                }
            }
        }
    }

    #[attrs::attrs]
    #[derive(Default)]
    pub struct Graph {
        pub nodes: Vec<Node>,
        pub edges: Vec<Edge>,
    }

    impl Graph {
        pub fn new() -> Self {
            Self::default()
        }

        pub fn with_nodes(mut self, nodes: &[Node]) -> Self {
            self.nodes = nodes.to_vec();
            self
        }

        pub fn with_edges(mut self, edges: &[Edge]) -> Self {
            self.edges = edges.to_vec();
            self
        }

        pub fn get_node(&self, node: &str) -> Option<&Node> {
            self.nodes.iter().find(|x| x.name == node)
        }
    }
}
