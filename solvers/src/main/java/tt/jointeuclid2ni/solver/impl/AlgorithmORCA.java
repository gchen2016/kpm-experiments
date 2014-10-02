package tt.jointeuclid2ni.solver.impl;

import java.util.Collection;
import java.util.LinkedList;

import org.jgrapht.DirectedGraph;

import rvolib.RVOSolver;
import tt.euclid2i.Line;
import tt.euclid2i.Point;
import tt.euclid2i.Region;
import tt.jointeuclid2ni.probleminstance.EarliestArrivalProblem;
import tt.jointtraj.solver.SearchResult;

public class AlgorithmORCA extends AbstractAlgorithm {

    public AlgorithmORCA() {
        super();
    }

    @Override
    public SearchResult solveProblem() {
        checkAllAgentsHaveSameSize(problem);

        long runtimeDeadlineNs = deadlineNs(params.runtimeDeadlineMs);
        double maxCost = Double.MAX_VALUE;
        
        float timeStep = 1.0f; // Experimenting here!
       
        int maxIterations = (int) (params.maxTime/timeStep);

        Collection<Region> obstacles = new LinkedList<Region>();
        obstacles.addAll(problem.getObstacles());
        obstacles.add(problem.getEnvironment().getBoundary());
        
        double bestGraphNodeSearchRadius;
        if (problem.getPlanningGraph() != null) {
        	bestGraphNodeSearchRadius = longestEdgeLength(problem.getPlanningGraph());
        } else {
        	bestGraphNodeSearchRadius = Double.MAX_VALUE;
        }
        
        int bodyRadius = problem.getBodyRadius(0); // we assume that all have the same radius
        float deconflictionTimeHorizon = bodyRadius * 2.1f;
		RVOSolver rvosolver = new RVOSolver(problem.getStarts(), problem.getTargets(), problem.getBodyRadius(0),
        		problem.getPlanningGraph(), 
        		bestGraphNodeSearchRadius, 
        		obstacles, 
        		timeStep, 
        		deconflictionTimeHorizon, /* neighborDist */
        		100, 					  /* maxNeighbors */
        		deconflictionTimeHorizon, /* deconfliction time horizon for other robots */
        		deconflictionTimeHorizon, /* deconfliction time horizon for obstacles */
        		1.0f, /* max speed */
        		params.verbose);

        return rvosolver.solve(maxIterations, runtimeDeadlineNs, maxCost);
    }

    private void checkAllAgentsHaveSameSize(EarliestArrivalProblem problem) {
        int nAgents = problem.nAgents();
        if (nAgents < 2)
            return;

        for (int i = 1; i < nAgents; i++) {
            int radiusA = problem.getBodyRadius(i - 1);
            int radiusB = problem.getBodyRadius(i);

            if (radiusA != radiusB)
                throw new RuntimeException("All agents have to have the same body radius!");
        }
    }

    private long deadlineNs(long runtimeDeadlineMs) {
        long ms = runtimeDeadlineMs - System.currentTimeMillis();
        return System.nanoTime() + ms * 1000000;
    }
    
    private double longestEdgeLength(DirectedGraph<Point, Line> planningGraph) {
    	double longestEdgeLength = 0;
    	for (Line edge : planningGraph.edgeSet()) {
			if (longestEdgeLength < edge.getDistance()) {
				longestEdgeLength = edge.getDistance();
			}
		}
    	
    	return longestEdgeLength;
	}

}
