pub struct SimpleLinkedList<T> {
    head: NodePtr<T>,
}

struct Node<T> {
    data: T,
    next: NodePtr<T>,
}

type NodePtr<T> = Option<Box<Node<T>>>;

impl<T> Default for SimpleLinkedList<T> {
    fn default() -> Self {
        Self { head: None }
    }
}

impl<T> SimpleLinkedList<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn len(&self) -> usize {
        let mut count = 0;
        let mut ptr = &self.head;

        while let Some(node) = ptr {
            count += 1;
            ptr = &node.next;
        }

        count
    }

    pub fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    pub fn push(&mut self, element: T) {
        let node = Node {
            data: element,
            next: self.head.take(),
        };
        self.head = Some(Box::new(node));
    }

    pub fn pop(&mut self) -> Option<T> {
        if let Some((head, tail)) = self.head.take().map(|b| (b.data, b.next)) {
            self.head = tail;
            Some(head)
        } else {
            None
        }
    }

    pub fn peek(&self) -> Option<&T> {
        self.head.as_ref().map(|b| &b.data)
    }

    pub fn rev(self) -> Self {
        let mut list = Self::new();
        let mut ptr = self;

        while let Some(data) = ptr.pop() {
            list.push(data);
        }

        list
    }
}

impl<T> std::iter::FromIterator<T> for SimpleLinkedList<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut list = Self::new();
        for i in iter {
            list.push(i);
        }

        list
    }
}

impl<T> Into<Vec<T>> for SimpleLinkedList<T> {
    fn into(mut self) -> Vec<T> {
        let mut v = Vec::new();

        while let Some(data) = self.pop() {
            v.push(data);
        }

        v.reverse();
        v
    }
}
