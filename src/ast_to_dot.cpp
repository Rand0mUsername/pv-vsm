/*
    Nikola Jovanovic - Matematicka gimnazija 4D
    Maturski rad iz informatike:
    Lambda racun i funkcionalno programiranje
    uz dodatni projekat : izrada kompajlera u programskom jeziku Haskell
    4.2015.

    Konverzija sintaksnog drveta (AST) iz formata koji parser
    daje kao izlaz u dot format koji GraphViz koristi pri
    generisanju graficke reprezentacije AST - a
*/

#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <vector>
#include <stack>

using namespace std;

string ast; // AST u ulaznom formatu
int astLen; // duzina AST - a

string currNode; // 1 token ~ 1 cvor
int nodeCount; // ukupan broj cvorova

// cvorove cemo mapirati u prirodne brojeve, a vector labels
// ce sadrzati informaciju potrebnu za labeliranje cvorova
// vector edges sadrzi ivice izlaznog grafa
// na vrhu stacka s se u svakom trenutku nalazi node koji je roditelj trenutnog
vector<string> labels;
vector< pair<int, int> > edges;
stack<int> s;

int main()
{
    // priprema input fajlova
    freopen("out/ast.txt", "r", stdin);
    freopen("out/ast.dot", "w", stdout);

    // pocetne vrednosti promenljivih
    nodeCount = -1;
    currNode = "";

    // ulaz
    getline(cin, ast);
    astLen = ast.size();

    // iteriramo po karakterima ulaznog AST-a
    for(int i=0; i<astLen; i++)
    {
        // zamena navodnika apostrofima radi lakse obrade u graphViz
        if(ast[i]=='"') ast[i] = '\'';

        // trenutni karakter je otvorena zagrada
        if(ast[i] == '(')
        {
            // ako smo ucitali node
            if(!currNode.empty())
            {
                // oznacimo mu label
                labels.push_back(currNode);

                // kako se podela na node-ove oslanja na zagrade
                // specijalan slucaj predstavljaju ORD tokeni
                // koje cemo posmatrati kao odvojen node (za razliku od True/False/Skip)
                if(currNode == "GT" || currNode == "LT" || currNode == "EQ")
                {
                    edges.push_back( pair<int, int>(s.top(), ++nodeCount) );
                }
                else s.push(++nodeCount); //ukoliko nije spec. slucaj dodamo novi node na vrh stacka

                // resetujemo string u kome gradimo label novog noda
                currNode = "";
            }

            //node koji cemo sledeci ucitati je sin node-a sa vrha stacka
            edges.push_back( pair<int, int>(s.top(), nodeCount + 1) );
        }
        else if(ast[i] == ')') //zatvorena zagrada
        {
            // ako smo ucitali node
            if(!currNode.empty())
            {
                // oznacimo mu label
                labels.push_back(currNode);

                // resetujemo string u kome gradimo label novog noda
                currNode = "";

                // dodajemo novi node na vrh stacka
                s.push(++nodeCount);
            }

            // node sa vrha stacka se uklanja
            s.pop();
        }
        else if(ast[i] != ' ')
        {
            // nastavljamo da gradimo label novog node-a, ignorisuci razmake
            currNode += ast[i];
        }
    }

    // output za graphViz


    // lista cvorova
    cout << "digraph{" << endl;
    for(int i=0; i<=nodeCount; i++)
    {
       cout << i << "[label=\"" << labels[i] << "\"];" << endl;
    }

    // lista grana
    for(int i=0; i<edges.size(); i++)
    {
       cout << edges[i].first << " -> " << edges[i].second << endl;
    }
    cout << "}" << endl;

    return 0;
}

