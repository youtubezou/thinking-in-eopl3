#include<stdio.h>
#include<stdlib.h>
#include<string>
#include<assert.h>
using namespace std;

struct Bintree 
{
    int val;
    Bintree* ltree;
    Bintree* rtree;
    
    Bintree(int v, Bintree* lt = NULL, Bintree* rt = NULL)
        : val(v), ltree(lt), rtree(rt){}

    Bintree* initialize(int v, Bintree* lt = NULL, Bintree* rt = NULL)
    {
        val = v; ltree = lt; rtree = rt;
        return (Bintree*)this;
    }

    string to_string(string indent)
    {
        char buf[1024] = { 0 };
        sprintf_s(buf,
            "%sBintree:\n"
            "%s  |->val:%d\n"
            "%s  |->ltree:%s\n"
            "%s  |->rtree:%s\n",
            indent.c_str(),
            indent.c_str(), val,
            indent.c_str(), ltree ? ltree->to_string(indent + indent).c_str() : "null",
            indent.c_str(), rtree ? rtree->to_string(indent + indent).c_str() : "null");
        return string((const char*)buf);
    }
};

typedef enum tagDIR {TOP, LEFT, RIGHT} DIR;
char* Dirs[] = {"TOP", "LEFT", "RIGHT"};
struct Context 
{
    DIR dir;
    int val;
    Bintree* subtree;
    Context* pre_ctxt;

    Context(DIR d, int v = 0, Bintree* t = NULL, Context* ctxt = NULL)
        :dir(d), val(v), subtree(t), pre_ctxt(ctxt){}

    string to_string(string indent)
    {
        char buf[1024] = { 0 };
        sprintf_s(buf,
            "%sCtxt:\n"
            "%s  |->dir:%s\n"
            "%s  |->val:%d\n"
            "%s  |->subtree:%s\n"
            "%s  |->ctxt:%s\n",
            indent.c_str(),
            indent.c_str(), Dirs[dir],
            indent.c_str(), val,
            indent.c_str(), subtree ? subtree->to_string(indent + indent).c_str() : "null",
            indent.c_str(), pre_ctxt ? pre_ctxt->to_string(indent + indent).c_str() : "null");
        return string(buf);
    }

};

//ZipperTree: That is Current Location 
struct ZipperTree 
{
    Bintree* tree;
    Context* ctxt;

    ZipperTree(Bintree* t, Context* c)
        :tree(t), ctxt(c){}

    ZipperTree* initialize(Bintree* t, Context* c)
    {
        tree = t;
        ctxt = c;
        return (ZipperTree*)this;
    }

    string to_string()
    {
        char buf[1024] = { 0 };
        string indent = "    ";
        sprintf_s(buf,
            "ZipperTree:\n"
            "  |->tree:%s\n"
            "  |->ctxt:%s\n",
            tree ? tree->to_string(indent).c_str() : "null",
            ctxt ? ctxt->to_string(indent).c_str() : "null");
        return string(buf);
    }

}; 

//-----------constructor-------------
#define leaf(val) (new Bintree(val))
#define tree(val, ltree, rtree) (new Bintree(val, ltree, rtree))
#define context_left(val, subtree, pre_ctxt) (new Context(LEFT, val, subtree, pre_ctxt))
#define context_right(val, subtree, pre_ctxt) (new Context(RIGHT, val, subtree, pre_ctxt))
#define context_top() (new Context(TOP))
#define zipper_tree(t, c) (new ZipperTree(t, c))
//-----------extractor--------------
#define ztree_to_node(ztree) ((ztree)->tree->val)
#define ztree_to_ltree(ztree) ((ztree)->tree->ltree)
#define ztree_to_rtree(ztree) ((ztree)->tree->rtree)
#define ztree_to_ctxt(ztree) ((ztree)->ctxt)
#define ctxt_to_dir(ctxt) ((ctxt)->dir)
#define ctxt_to_node(ctxt) ((ctxt)->val)
#define ctxt_to_subtree(ctxt) ((ctxt)->subtree)
#define ctxt_to_pre_ctxt(ctxt) ((ctxt)->pre_ctxt)
#define ztree_to_parent_dir(ztree) (ctxt_to_dir(ztree_to_ctxt(ztree)))
#define ztree_to_parent_node(ztree) (ctxt_to_node(ztree_to_ctxt(ztree)))
#define ztree_to_parent_child(ztree) (ctxt_to_subtree(ztree_to_ctxt(ztree)))
#define ztree_to_parent_ctxt(ztree) (ctxt_to_pre_ctxt(ztree_to_ctxt(ztree)))
//-------------predicate------------
bool at_leaf(ZipperTree* ztree) { return (NULL == ztree_to_ltree(ztree)); }
bool at_root(ZipperTree* ztree) { return (TOP == ztree_to_parent_dir(ztree)); }
//-----------bintree-zipper----------
ZipperTree* number_to_ztree(int val) { return zipper_tree(leaf(val), context_top()); }
int current_element(ZipperTree* ztree) { return ztree_to_node(ztree); }

//不需要创建ztree节点,向下移动时(即move_left/right)
//ZipperTree中：
//  tree:向上移动时分配，向下移动时释放
//  Context:向下移动时分配，向上移动时释放
ZipperTree* move_to_left(ZipperTree* ztree)
{
    Bintree* old_tree = ztree->tree;
    ZipperTree* t = ztree->initialize(ztree_to_ltree(ztree),
                            context_right(ztree_to_node(ztree),
                                            ztree_to_rtree(ztree),
                                            ztree_to_ctxt(ztree)));
    delete old_tree;
    return t;
}

ZipperTree* move_to_right(ZipperTree* ztree)
{
    Bintree* old_tree = ztree->tree;
    ZipperTree* t = ztree->initialize(ztree_to_rtree(ztree),
                            context_left(ztree_to_node(ztree),
                                            ztree_to_ltree(ztree),
                                            ztree_to_ctxt(ztree)));
    delete old_tree;
    return t;
}

ZipperTree* insert_to_left(int val, ZipperTree* ztree)
{
    Bintree* t = leaf(val);
    t->ltree = ztree_to_ltree(ztree);
    ztree->tree->ltree = t;
    return ztree;
}

ZipperTree* insert_to_right(int val, ZipperTree* ztree)
{
    Bintree* t = leaf(val);
    t->rtree = ztree_to_rtree(ztree);
    ztree->tree->rtree = t;
    return ztree;
}

//只需要更新出节点的指针
ZipperTree* move_up(ZipperTree* ztree)
{
    bool left_to_parent = (ztree_to_parent_dir(ztree) == LEFT);
    int parent_val = ztree_to_parent_node(ztree);
    Bintree* ltree = left_to_parent ? ztree_to_parent_child(ztree)
                                    : ztree->tree;
    Bintree* rtree = left_to_parent ? ztree->tree
                                    : ztree_to_parent_child(ztree);

    ztree->tree = tree(parent_val, ltree, rtree);
    Context* old_ctxt = ztree->ctxt;
    ztree->ctxt = ztree_to_parent_ctxt(ztree);
    delete old_ctxt;
    return ztree;
}



void test_constuctors()
{
    printf("--------------top--------------\n");
    ZipperTree* top = number_to_ztree(12);
    printf("%s\n", top->to_string().c_str());
    assert(at_leaf(top) && at_root(top));

    printf("--------------constructors--------------\n");
    ZipperTree* c1 = zipper_tree(leaf(13),
        context_left(27, leaf(14), context_top()));
    printf("%s\n", c1->to_string().c_str());

    printf("--------------insert--------------\n");
    ZipperTree* c2 = insert_to_right(5, insert_to_left(7, c1));
    printf("%s\n", c2->to_string().c_str());

    printf("--------------move-left--------------\n");
    //ZipperTree* c3 = move_to_left(c2);
    //printf("%s\n", c3->to_string().c_str());

    printf("--------------move-right--------------\n");
    ZipperTree* c4 = move_to_right(c2);
    printf("%s\n", c4->to_string().c_str());

    printf("--------------move-up--------------\n");
    ZipperTree* c5 = move_up(c4);
    printf("%s\n", c5->to_string().c_str());

    printf("--------------move-up-to-top--------------\n");
    ZipperTree* c6 = move_up(c5);
    printf("%s\n", c6->to_string().c_str());
    assert(at_root(c6) && !at_leaf(c6));

    printf("-------------move-to-c4---------------\n");
    ZipperTree* c7 = move_to_right(move_to_right(c6));
    printf("%s\n", c7->to_string().c_str());
}



int main(void)
{
    test_constuctors();

    getchar();
    return 0;
}


