#include <iostream>
#include <random>
#include <fstream>
#include <vector>
#include <numeric>
#include <string>
#include <algorithm>

using namespace std;

void query1(int type, vector<int> &brute, ofstream &test, ofstream &res, mt19937 &gen, uniform_int_distribution<> &numbersGen)
{
    //insert(position, value)
    uniform_int_distribution<> posGen(0, (int)brute.size());
    int position = posGen(gen);
    int value = numbersGen(gen);
    test << type << ' ' << position << ' ' << value << '\n';
    brute.insert(brute.begin() + position, value);
}

void query2(int type, vector<int> &brute, ofstream &test, ofstream &res, mt19937 &gen, uniform_int_distribution<> &numbersGen)
{
    //getSum(l, r) -> sum
    if (brute.empty())
        return;
    uniform_int_distribution<> posGen(0, (int)brute.size() - 1);
    int l = posGen(gen);
    uniform_int_distribution<> posGen2(l, (int)brute.size() - 1);
    int r = posGen2(gen);
    test << type << ' ' << l << ' ' << r << '\n';
    int sum = 0;
    for (int i = l; i <= r; ++i)
        sum += brute[i];
    res << sum << '\n';
}

void query3(int type, vector<int> &brute, ofstream &test, ofstream &res, mt19937 &gen, uniform_int_distribution<> &numbersGen)
{
    //getValue(position) -> value
    if (brute.empty())
        return;
    uniform_int_distribution<> posGen(0, (int)brute.size() - 1);
    int position = posGen(gen);
    test << type << ' ' << position << '\n';
    res << brute[position] << '\n';
}

void query4(int type, vector<int> &brute, ofstream &test, ofstream &res, mt19937 &gen, uniform_int_distribution<> &numbersGen)
{
    //getArray() -> [0..size)
    if (brute.empty())
        return;
    test << type << '\n';
    for (int i = 0; i < brute.size(); ++i)
        res << brute[i] << " \n"[i + 1 == brute.size()];
}

void query5(int type, vector<int> &brute, ofstream &test, ofstream &res, mt19937 &gen, uniform_int_distribution<> &numbersGen)
{
    //getRange(l, r) -> [l..r]
    if (brute.empty())
        return;
    uniform_int_distribution<> posGen(0, (int)brute.size() - 1);
    int l = posGen(gen);
    uniform_int_distribution<> posGen2(l, (int)brute.size() - 1);
    int r = posGen2(gen);
    test << type << ' ' << l << ' ' << r << '\n';
    for (int i = l; i <= r; ++i)
        res << brute[i] << " \n"[i == r];
}

void query6(int type, vector<int> &brute, ofstream &test, ofstream &res, mt19937 &gen, uniform_int_distribution<> &numbersGen)
{
    //setRange(l, r, value)
    if (brute.empty())
        return;
    uniform_int_distribution<> posGen(0, (int)brute.size() - 1);
    int l = posGen(gen);
    uniform_int_distribution<> posGen2(l, (int)brute.size() - 1);
    int r = posGen2(gen);
    int value = numbersGen(gen);
    test << type << ' ' << l << ' ' << r << ' ' << value << '\n';
    for (int i = l; i <= r; ++i)
        brute[i] = value;
}

void query7(int type, vector<int> &brute, ofstream &test, ofstream &res, mt19937 &gen, uniform_int_distribution<> &numbersGen)
{
    //setValue(position, value)
    if (brute.empty())
        return;
    uniform_int_distribution<> posGen(0, (int)brute.size() - 1);
    int position = posGen(gen);
    int value = numbersGen(gen);
    test << type << ' ' << position << ' ' << value << '\n';
    brute[position] = value;
}

void query8(int type, vector<int> &brute, ofstream &test, ofstream &res, mt19937 &gen, uniform_int_distribution<> &numbersGen)
{
    //set(value)
    int value = numbersGen(gen);
    test << type << ' ' << value << '\n';
    for (int &i : brute)
        i = value;
}

void query9(int type, vector<int> &brute, ofstream &test, ofstream &res, mt19937 &gen, uniform_int_distribution<> &numbersGen)
{
    //addRange(l, r, value)
    if (brute.empty())
        return;
    uniform_int_distribution<> posGen(0, (int)brute.size() - 1);
    int l = posGen(gen);
    uniform_int_distribution<> posGen2(l, (int)brute.size() - 1);
    int r = posGen2(gen);
    int value = numbersGen(gen);
    test << type << ' ' << l << ' ' << r << ' ' << value << '\n';
    for (int i = l; i <= r; ++i)
        brute[i] += value;
}

void query10(int type, vector<int> &brute, ofstream &test, ofstream &res, mt19937 &gen, uniform_int_distribution<> &numbersGen)
{
    //addValue(position, value)
    if (brute.empty())
        return;
    uniform_int_distribution<> posGen(0, (int)brute.size() - 1);
    int position = posGen(gen);
    int value = numbersGen(gen);
    test << type << ' ' << position << ' ' << value << '\n';
    brute[position] += value;
}

void query11(int type, vector<int> &brute, ofstream &test, ofstream &res, mt19937 &gen, uniform_int_distribution<> &numbersGen)
{
    //add(value)
    int value = numbersGen(gen);
    test << type << ' ' << value << '\n';
    for (int &i : brute)
        i += value;
}

void query12(int type, vector<int> &brute, ofstream &test, ofstream &res, mt19937 &gen, uniform_int_distribution<> &numbersGen)
{
    //reverseRange(l, r)
    if (brute.empty())
        return;
    uniform_int_distribution<> posGen(0, (int)brute.size() - 1);
    int l = posGen(gen);
    uniform_int_distribution<> posGen2(l, (int)brute.size() - 1);
    int r = posGen2(gen);
    test << type << ' ' << l << ' ' << r << '\n';
    reverse(brute.begin() + l, brute.begin() + r + 1);
}

void query13(int type, vector<int> &brute, ofstream &test, ofstream &res, mt19937 &gen, uniform_int_distribution<> &numbersGen)
{
    //reverse()
    test << type << '\n';
    reverse(brute.begin(), brute.end());
}

void query14(int type, vector<int> &brute, ofstream &test, ofstream &res, mt19937 &gen, uniform_int_distribution<> &numbersGen)
{
    //length() -> length
    test << type << '\n';
    res << brute.size() << '\n';
}

void query15(int type, vector<int> &brute, ofstream &test, ofstream &res, mt19937 &gen, uniform_int_distribution<> &numbersGen)
{
    //pushFront(value)
    int value = numbersGen(gen);
    test << type << ' ' << value << '\n';
    brute.insert(brute.begin(), value);
}

void query16(int type, vector<int> &brute, ofstream &test, ofstream &res, mt19937 &gen, uniform_int_distribution<> &numbersGen)
{
    //pushBack(value)
    int value = numbersGen(gen);
    test << type << ' ' << value << '\n';
    brute.push_back(value);
}

void query17(int type, vector<int> &brute, ofstream &test, ofstream &res, mt19937 &gen, uniform_int_distribution<> &numbersGen)
{
    //eraseRange(l, r)
    if (brute.empty())
        return;
    uniform_int_distribution<> posGen(0, (int)brute.size() - 1);
    int l = posGen(gen);
    uniform_int_distribution<> posGen2(l, (int)brute.size() - 1);
    int r = posGen2(gen);
    test << type << ' ' << l << ' ' << r << '\n';
    brute.erase(brute.begin() + l, brute.begin() + r + 1);
}

void query18(int type, vector<int> &brute, ofstream &test, ofstream &res, mt19937 &gen, uniform_int_distribution<> &numbersGen)
{
    //erase(position)
    if (brute.empty())
        return;
    uniform_int_distribution<> posGen(0, (int)brute.size() - 1);
    int position = posGen(gen);
    test << type << ' ' << position << '\n';
    brute.erase(brute.begin() + position);
}

void query19(int type, vector<int> &brute, ofstream &test, ofstream &res, mt19937 &gen, uniform_int_distribution<> &numbersGen)
{
    //popFront()
    if (brute.empty())
        return;
    test << type << '\n';
    brute.erase(brute.begin());
}

void query20(int type, vector<int> &brute, ofstream &test, ofstream &res, mt19937 &gen, uniform_int_distribution<> &numbersGen)
{
    //popBack()
    if (brute.empty())
        return;
    test << type << '\n';
    brute.pop_back();
}

int main(int argc, char *argv[])
{
    string testID = argv[1];
    int seed = atoi(argv[2]);
    int minVal = atoi(argv[3]);
    int maxVal = atoi(argv[4]);
    int queries = atoi(argv[5]);
    int len = atoi(argv[6]);
    vector<int> queryTypes;
    if (argc == 7)
    {
        queryTypes.resize(20);
        iota(queryTypes.begin(), queryTypes.end(), 1);
    }
    else
    {
        for (char i : string(argv[7]))
        {
            if ('0' <= i && i <= '9')
                queryTypes.push_back(i - '0');
            else
                queryTypes.push_back(i - 'A' + 10);
        }
    }
    mt19937 gen(seed);
    vector<int> brute(len);
    uniform_int_distribution<> numbersGen(minVal, maxVal);
    uniform_int_distribution<> queriesGen(0, (int)queryTypes.size() - 1);
    ofstream test("in" + testID + ".txt");
    ofstream res("out" + testID + ".txt");
    for (int &i : brute)
    {
        i = numbersGen(gen);
        test << i << ' ';
    }
    test << '\n';
    for (int i = 0; i < queries; ++i)
    {
        int type = queryTypes[queriesGen(gen)];
        switch (type)
        {
            case 1:
                query1(type, brute, test, res, gen, numbersGen);
                break;
                
            case 2:
                query2(type, brute, test, res, gen, numbersGen);
                break;
                
            case 3:
                query3(type, brute, test, res, gen, numbersGen);
                break;
                
            case 4:
                query4(type, brute, test, res, gen, numbersGen);
                break;
                
            case 5:
                query5(type, brute, test, res, gen, numbersGen);
                break;
                
            case 6:
                query6(type, brute, test, res, gen, numbersGen);
                break;
                
            case 7:
                query7(type, brute, test, res, gen, numbersGen);
                break;
                
            case 8:
                query8(type, brute, test, res, gen, numbersGen);
                break;
                
            case 9:
                query9(type, brute, test, res, gen, numbersGen);
                break;
                
            case 10:
                query10(type, brute, test, res, gen, numbersGen);
                break;
                
            case 11:
                query11(type, brute, test, res, gen, numbersGen);
                break;
                
            case 12:
                query12(type, brute, test, res, gen, numbersGen);
                break;
                
            case 13:
                query13(type, brute, test, res, gen, numbersGen);
                break;
                
            case 14:
                query14(type, brute, test, res, gen, numbersGen);
                break;
                
            case 15:
                query15(type, brute, test, res, gen, numbersGen);
                break;
                
            case 16:
                query16(type, brute, test, res, gen, numbersGen);
                break;
                
            case 17:
                query17(type, brute, test, res, gen, numbersGen);
                break;
                
            case 18:
                query18(type, brute, test, res, gen, numbersGen);
                break;
                
            case 19:
                query19(type, brute, test, res, gen, numbersGen);
                break;
                
            case 20:
                query20(type, brute, test, res, gen, numbersGen);
                break;
        }
    }
    query2(2, brute, test, res, gen, numbersGen);
    return 0;
}
