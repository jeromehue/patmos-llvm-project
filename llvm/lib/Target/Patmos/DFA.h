//===---------------------------- DFA.h -----------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides the interface for Data-Flow Analysis Pass.
//
//===----------------------------------------------------------------------===//

#pragma once

#include "llvm/InitializePasses.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"

#include <deque>
#include <map>
#include <utility>
#include <vector>
#include <set>

namespace llvm {


class Info {
  public:
    Info() {}
    Info(const Info& other) {}
    virtual ~Info() {};

    // Print the information
    virtual void print() = 0;

    static bool equals(Info * info1, Info * info2);
    
    static Info* join(Info * info1, Info * info2, Info * result);
};

/*
 * This is the base template class to represent the generic dataflow analysis framework
 * For a specific analysis, you need to create a sublcass of it.
 */
template <class Info, bool Direction>
class DataFlowAnalysis {

  private:
		typedef std::pair<unsigned, unsigned> Edge;
		// Index to instruction map
		std::map<unsigned, Instruction *> IndexToInstr;
		// Instruction to index map
		std::map<Instruction *, unsigned> InstrToIndex;
		// Edge to information map
		std::map<Edge, Info *> EdgeToInfo;
		// The bottom of the lattice
	    Info Bottom;
	    // The initial state of the analysis
		Info InitialState;
		// EntryInstr points to the first instruction to be processed in the analysis
		Instruction * EntryInstr;

    // Should be a vector
    unsigned VarLoopBoundInstr;


		/*
		 * Assign an index to each instruction.
		 * The results are stored in InstrToIndex and IndexToInstr.
		 * A dummy node (nullptr) is added. It has index 0. This node has only one outgoing edge to EntryInstr.
		 * The information of this edge is InitialState.
		 * Any real instruction has an index > 0.
		 */
		void assignIndiceToInstrs(Function * F) {

			// Dummy instruction null has index 0;
			// Any real instruction's index > 0.
			InstrToIndex[nullptr] = 0;
			IndexToInstr[0] = nullptr;

			unsigned counter = 1;
			for (inst_iterator I = inst_begin(F), E = inst_end(F); I != E; ++I) {
				Instruction * instr = &*I;
				InstrToIndex[instr] = counter;
				IndexToInstr[counter] = instr;
				counter++;
			}

			return;
		}

		/*
		 * Utility function:
		 *   Get incoming edges of the instruction identified by index.
		 *   IncomingEdges stores the indices of the source instructions of the incoming edges.
		 */
		void getIncomingEdges(unsigned index, std::vector<unsigned> * IncomingEdges) {
			assert(IncomingEdges->size() == 0 && "IncomingEdges should be empty.");

			for (auto const &it : EdgeToInfo) {
				if (it.first.second == index)
					IncomingEdges->push_back(it.first.first);
			}

			return;
		}

		/*
		 * Utility function:
		 *   Get incoming edges of the instruction identified by index.
		 *   OutgoingEdges stores the indices of the destination instructions of the outgoing edges.
		 */
		void getOutgoingEdges(unsigned index, std::vector<unsigned> * OutgoingEdges) {
			assert(OutgoingEdges->size() == 0 && "OutgoingEdges should be empty.");

			for (auto const &it : EdgeToInfo) {
				if (it.first.first == index)
					OutgoingEdges->push_back(it.first.second);
			}

			return;
		}

		/*
		 * Utility function:
		 *   Insert an edge to EdgeToInfo.
		 *   The default initial value for each edge is bottom.
		 */
		void addEdge(Instruction * src, Instruction * dst, Info * content) {
			Edge edge = std::make_pair(InstrToIndex[src], InstrToIndex[dst]);
			if (EdgeToInfo.count(edge) == 0)
				EdgeToInfo[edge] = content;
			return;
		}

		/*
		 * Initialize EdgeToInfo and EntryInstr for a forward analysis.
		 */
		void initializeForwardMap(Function * func) {
			assignIndiceToInstrs(func);

			for (Function::iterator bi = func->begin(), e = func->end(); bi != e; ++bi) {
				BasicBlock * block = &*bi;

				Instruction * firstInstr = &(block->front());

				// Initialize incoming edges to the basic block
				for (auto pi = pred_begin(block), pe = pred_end(block); pi != pe; ++pi) {
					BasicBlock * prev = *pi;
					Instruction * src = (Instruction *)prev->getTerminator();
					Instruction * dst = firstInstr;
					addEdge(src, dst, &Bottom);
				}

				// If there is at least one phi node, add an edge from the first phi node
				// to the first non-phi node instruction in the basic block.
				if (isa<PHINode>(firstInstr)) {
					addEdge(firstInstr, block->getFirstNonPHI(), &Bottom);
				}

				// Initialize edges within the basic block
				for (auto ii = block->begin(), ie = block->end(); ii != ie; ++ii) {
					Instruction * instr = &*ii;
					if (isa<PHINode>(instr))
						continue;
					if (instr == (Instruction *)block->getTerminator())
						break;
					Instruction * next = instr->getNextNode();
					addEdge(instr, next, &Bottom);
				}

				// Initialize outgoing edges of the basic block
				Instruction * term = (Instruction *)block->getTerminator();
				for (auto si = succ_begin(block), se = succ_end(block); si != se; ++si) {
					BasicBlock * succ = *si;
					Instruction * next = &(succ->front());
					addEdge(term, next, &Bottom);
				}

			}

			EntryInstr = (Instruction *) &((func->front()).front());
			addEdge(nullptr, EntryInstr, &InitialState);

			return;
		}

		void initializeBackwardMap(Function* func) {
		}

    /*
     * The flow function.
     *   Instruction I: the IR instruction to be processed.
     *   std::vector<unsigned> & IncomingEdges: the vector of the indices of the source instructions of the incoming edges.
     *   std::vector<unsigned> & IncomingEdges: the vector of indices of the source instructions of the outgoing edges.
     *   std::vector<Info *> & Infos: the vector of the newly computed information for each outgoing eages.
     *
     */
    virtual void flowfunction(Instruction* I,
    													std::vector<unsigned> &IncomingEdges,
															std::vector<unsigned> &OutgoingEdges,
															std::vector<Info *> &Infos) = 0;

  public:
    DataFlowAnalysis(Info & bottom, Info & initialState) :
    								 Bottom(bottom), InitialState(initialState),EntryInstr(nullptr) {}

    virtual ~DataFlowAnalysis() {}

    std::map<Edge, Info *> getEdgeToInfo() 
    {
    	return EdgeToInfo;
    }

    std::map<Instruction *, unsigned> getInstrToIndex() 
    {
    	return InstrToIndex;
    }

    std::map<unsigned, Instruction *> getIndexToInstr() 
    {
    	return IndexToInstr;
    }

    // Print out the analysis results.
    void print() {
			for (auto const &it : EdgeToInfo) {
				errs() << "Edge " << it.first.first << "->" "Edge " << it.first.second << ":";
				(it.second)->print();
			}
    }


    bool isCallToMyFunction(llvm::Instruction* instr) {
      if (llvm::CallInst* callInst = llvm::dyn_cast<llvm::CallInst>(instr)) {
        // Check if the called function is "myFunction"
        llvm::Function* calledFunction = callInst->getCalledFunction();
        if (calledFunction && calledFunction->getName() == "llvm.loop.varbound") {
            return true;
        }
      }
      return false;
    }

    /*
     * This function implements the work list algorithm in the following steps:
     * (1) Initialize info of each edge to bottom
     * (2) Initialize the worklist
     * (3) Compute until the worklist is empty
     */
    void runWorklistAlgorithm(Function * func) {

    	std::deque<unsigned> worklist;

    	// (1) Initialize info of each edge to bottom
    	if (Direction)
    		initializeForwardMap(func);
    	else
    		initializeBackwardMap(func);


      // Debug: Finding a call to LLVM Varbound.
      for(auto& Instr: InstrToIndex) {
        llvm::Instruction* Instruction = Instr.first;
        if (Instruction != nullptr) {
          if(isCallToMyFunction(Instr.first)) {
            //llvm::dbgs() << " Found a call to llvm.loop.varbound(), at index " << InstrToIndex[Instruction] << "\n";
          }
        }
      }

    	// Initialize the work list
    	std::set<unsigned> nodeSet;

    	for (auto it = EdgeToInfo.begin(); it != EdgeToInfo.end(); ++it) {
    		Edge edge = it->first;

    		if (edge.first == 0)
    			continue;

    		// Add all instructions nodes to worklist
    		if (nodeSet.count(edge.first) == 0) {
    			worklist.push_back(edge.first);
    			nodeSet.insert(edge.first);
    		}

    		if (nodeSet.count(edge.second) == 0) {
    			worklist.push_back(edge.second);
    			nodeSet.insert(edge.second);
    		}
    	}

    	// Compute until the work list is empty
    	while (worklist.size() > 0) {
    		std::vector<unsigned> incomingEdges;
    		std::vector<unsigned> outgoingEdges;
    		std::vector<Info *> infos;
    		
        // pop element from worklist
    		unsigned InstrIdx = worklist.front();
    		worklist.pop_front();
    		Instruction * Instr = IndexToInstr[InstrIdx];

    		getIncomingEdges(InstrIdx, &incomingEdges);
    		getOutgoingEdges(InstrIdx, &outgoingEdges);

    		flowfunction(Instr, incomingEdges, outgoingEdges, infos);

    		for (size_t i = 0; i < infos.size(); ++i) {
    			Info * newInfo = new Info();
    			Edge outEdge = std::make_pair(InstrIdx, outgoingEdges[i]);

    			Info::join(infos[i], EdgeToInfo[outEdge], newInfo);

    			if (!Info::equals(EdgeToInfo[outEdge], newInfo)) {
    				EdgeToInfo[outEdge] = newInfo;
    				worklist.push_back(outEdge.second);
    			}
    		}
    	}
    }
};
}
