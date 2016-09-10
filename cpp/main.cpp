#include <windows.h>
#include <ctime>
#include <iostream>
#include <algorithm>

template <class T>
class Heap {
    static const int MaxSize = 1e5;

    std::vector<T> data;

public:
    Heap() {
    }

    Heap(const Heap &heap)
        : data(heap.data) {
    }

    Heap(Heap &&heap)
        : data(std::move(heap.data)) {
    }

    void push(T e) {
        data.push_back(e);
        siftUp(data.size() - 1);

        if (data.size() == MaxSize)
            data.pop_back();
    }

    T pop() {
        T ret = data[0];
        T end = data.back();

        data.pop_back();

        if (!data.empty()) {
            data[0] = end;
            siftDown(0);
        }

        return ret;
    }

    bool empty() const {
        return data.empty();
    }

private:
    void siftUp(int pos) {
        if (pos == 0)
            return;

        int parentPos = (pos - 1) / 2;

        T cur = data[pos];
        T parent = data[parentPos];

        if (cur < parent) {
            data[pos] = parent;
            data[parentPos] = cur;

            siftUp(parentPos);
        }
    }

    void siftDown(int pos) {
        int leftPos = pos * 2 + 1;
        int rightPos = pos * 2 + 2;

        int swapPos = -1;

        if (leftPos < (int)data.size() && data[leftPos] < data[pos])
            swapPos = leftPos;

        if (rightPos < (int)data.size() && data[rightPos] < data[leftPos] && data[rightPos] < data[pos])
            swapPos = rightPos;

        if (swapPos != -1) {
            std::swap(data[pos], data[swapPos]);
            siftDown(swapPos);
        }
    }
};

int index(int x, int y) {
    return y * 4 + x;
}

int xCoord(int i) {
    return i % 4;
}

int yCoord(int i) {
    return i / 4;
}

class Grid {
    char data[16];

public:
    static Grid random() {
        Grid grid({1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0});

        for (int i = 0; i < 250; i++) {
            std::vector<int> moves = grid.validMoves();
            grid = grid.applyMove(moves[rand() % moves.size()]);
        }

        return grid;
    }

    Grid() {
    }

    Grid(const std::initializer_list<int> &list) {
        std::initializer_list<int>::iterator it = list.begin();

        for (int i = 0; i < 16; i++)
            data[i] = *it++;
    }

    Grid(const Grid &grid) {
        std::copy(grid.data, grid.data + 16, data);
    }

    const char *getData() const {
        return data;
    }

    std::vector<int> validMoves() const {
        int zero = findZero();
        int x = xCoord(zero), y = yCoord(zero);

        std::vector<int> moves;

        if (x > 0)
            moves.push_back(index(x - 1, y));

        if (x < 3)
            moves.push_back(index(x + 1, y));

        if (y > 0)
            moves.push_back(index(x, y - 1));

        if (y < 3)
            moves.push_back(index(x, y + 1));

        std::random_shuffle(moves.begin(), moves.end());

        return moves;
    }

    Grid applyMove(int move) const {
        int zero = findZero();

        Grid newGrid(*this);
        std::swap(newGrid.data[zero], newGrid.data[move]);

        return newGrid;
    }

    int heuristic() const {
        int sum = 0;

        for (int x = 0; x < 4; x++) {
            for (int y = 0; y < 4; y++) {
                int value = data[index(x, y)];

                if (value == 0)
                    continue;

                sum += abs(x - xCoord(value - 1)) + abs(y - yCoord(value - 1));
            }
        }

        return sum;
    }

    int findZero() const {
        for (int i = 0; i < 16; i++)
            if (data[i] == 0)
                return i;

        return 0;
    }

    void print() {
        for (int i = 0; i < 16; i++) {
            if (i % 4 == 0)
                std::cout << "\n";
            else
                std::cout << " ";

            std::cout << (int)data[i];
        }

        std::cout << "\n";
    }

    bool operator<(const Grid &g) const {
        for (int i = 0; i < 16; i++)
            if (data[i] != g.data[i])
                return data[i] < g.data[i];

        return false;
    }
};

class State {
    Grid grid;
    std::vector<std::pair<int, int>> moves;
    int heuristic, cost;

public:
    State(const Grid &grid, const std::vector<std::pair<int, int>> &moves)
        : grid(grid), moves(moves) {
        heuristic = grid.heuristic();
        cost = heuristic + moves.size();
    }

    State(const State &state) {
        *this = state;
    }

    State &operator=(const State &state) {
        grid = state.grid;
        moves = state.moves;
        heuristic = state.heuristic;
        cost = state.cost;

        return *this;
    }

    bool operator<(const State &s) const {
        //return cost < s.cost;
        return cost != s.cost ? cost < s.cost : grid < s.grid;
    }

    const Grid &getGrid() const {
        return grid;
    }

    std::vector<std::pair<int, int>> getMoves() const {
        return moves;
    }

    bool solved() const {
        return heuristic == 0;
    }

    State applyMove(int move) const {
        int zero = grid.findZero();

        const Grid &newGrid = grid.applyMove(move);

        std::vector<std::pair<int, int>> newMoves = moves;
        newMoves.push_back(std::make_pair(move, index(xCoord(zero), yCoord(zero))));

        return State(newGrid, newMoves);
    }
};

std::vector<std::pair<int, int>> solve(const Grid &grid) {
    Heap<State> frontier;
    frontier.push(State(grid, std::vector<std::pair<int, int>>()));

    while (!frontier.empty()) {
        State curState = frontier.pop();

        if (curState.solved())
            return curState.getMoves();

        const std::vector<int> &candidates = curState.getGrid().validMoves();
        const std::vector<std::pair<int, int>> &moves = curState.getMoves();

        for (int move : candidates)
            if (moves.empty() || move != moves.back().second)
                frontier.push(curState.applyMove(move));
    }

    return std::vector<std::pair<int, int>>();
}

int main() {
    srand(time(0));

    //Grid grid({14, 6, 9, 2, 10, 4, 13, 3, 1, 7, 0, 12, 5, 15, 11, 8});
    Grid grid({1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 10, 0, 12, 13, 14, 15});

    const std::vector<std::pair<int, int>> &moves = solve(grid);

    for (std::pair<int, int> move : moves) {
        grid.print();
        grid = grid.applyMove(move.first);

        //Sleep(100);
        //system("cls");
    }

    grid.print();

    std::cout << "\n";

    //while (true) {
    //    Grid g = Grid::random();
    //    const std::vector<std::pair<int, int>> moves = solve(g);
    //    std::cout << moves.size() << "\n";
    //}

    return 0;
}
