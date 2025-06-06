document.getElementById("evaluateBtn").addEventListener("click", function(event) {
    event.preventDefault();

    const hand = document.getElementById("handInput").value;
    const fs_sz = parseInt(document.getElementById("fsSzInput").value, 10);
    const dist = parseInt(document.getElementById("distInput").value, 10);

    const payload = {
        card_str: hand,
        fs_sz: fs_sz,
        dist: dist
    };

    fetch("/api/evaluate", {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
                "Accept": "application/json"
            },
            body: JSON.stringify(payload)
        })
        .then(response => response.json())
        .then(data => {
            console.log("Evaluation result:", data);
            const resultDiv = document.getElementById("result");
            resultDiv.style.display = "block";
            resultDiv.innerHTML = `<h3>Hand Type: ${data.handType}</h3>`;
        })
        .catch(error => {
            console.error("Evaluation error:", error);
            alert("Error evaluating hand: " + error.message);
        });
});

document.getElementById('simulateBtn').addEventListener('click', function(event) {
    event.preventDefault();

    const remain_deck = document.getElementById('remainDeckInput').value;
    const fs_sz = parseInt(document.getElementById("fsSzInput").value, 10);
    const dist = parseInt(document.getElementById("distInput").value, 10);
    const remain_card = document.getElementById('remainCardInput').value;
    const numSim = document.getElementById('numSimInput').value;
    const numDraw = document.getElementById('numDrawInput').value;
    const is_logged = document.getElementById('enableLogging').checked;

    console.log('Simulation parameters:', {
        remain_deck,
        remain_card,
        fs_sz,
        dist,
        numSim,
        numDraw,
        is_logged
    });

    const payload = {
        remain_deck: remain_deck,
        remain_hand: remain_card,
        sz: fs_sz,
        distance: dist,
        num_sim: parseInt(numSim),
        num_draw: parseInt(numDraw),
        is_logged: is_logged
    };

    console.log('Request payload:', payload);

    fetch('/api/simulate', {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
            'Accept': 'application/json'
        },
        body: JSON.stringify(payload)
    })
    .then(response => {
        console.log('Response status:', response.status);
        if (!response.ok) {
            return response.text().then(text => {
                throw new Error(`HTTP error! status: ${response.status}, body: ${text}`);
            });
        }
        return response.json();
    })
    .then(data => {
        console.log('Response data:', data);
        displaySimulationResults(data);
    })
    .catch(error => {
        console.error('Exception during simulation:', error);
        console.error('Error details:', {
            name: error.name,
            message: error.message,
            stack: error.stack
        });
        document.getElementById('simulationResult').innerHTML = `<div class="error">Error: ${error.message}</div>`;
    });
});

function displaySimulationResults(data) {
    console.log('Processing simulation results:', data);
    
    // Display hand type stats with percentages
    const handTypeTotal = data.handTypeStats.reduce((sum, [_, count]) => sum + count, 0);
    console.log('Hand type total:', handTypeTotal);
    
    const handTypeHtml = data.handTypeStats
        .map(([type, count]) => {
            const percentage = ((count / handTypeTotal) * 100).toFixed(1);
            console.log(`Hand type ${type}: count=${count}, percentage=${percentage}%`);
            return `
                <div class="stat-item">
                    <span class="stat-label">${type}</span>
                    <span class="stat-value">
                        ${count} <span class="percentage">(${percentage}%)</span>
                    </span>
                </div>`;
        })
        .join('');
    document.getElementById('handTypeStatsContent').innerHTML = handTypeHtml;

    // Display enhancement stats
    console.log('Processing enhancement stats:', data.enhancementStats);
    const enhancementHtml = data.enhancementStats
        .map(([type, count]) => {
            console.log(`Enhancement ${type}: count=${count}`);
            return `
                <div class="stat-item">
                    <span class="stat-label">${type}</span>
                    <span class="stat-value">${count.toFixed(2)}</span>
                </div>`;
        })
        .join('');
    document.getElementById('enhancementStatsContent').innerHTML = enhancementHtml;

    // Display edition stats
    console.log('Processing edition stats:', data.editionStats);
    const editionHtml = data.editionStats
        .map(([type, count]) => {
            console.log(`Edition ${type}: count=${count}`);
            return `
                <div class="stat-item">
                    <span class="stat-label">${type}</span>
                    <span class="stat-value">${count.toFixed(2)}</span>
                </div>`;
        })
        .join('');
    document.getElementById('editionStatsContent').innerHTML = editionHtml;

    // Display seal stats
    console.log('Processing seal stats:', data.sealStats);
    const sealHtml = data.sealStats
        .map(([type, count]) => {
            console.log(`Seal ${type || 'None'}: count=${count}`);
            return `
                <div class="stat-item">
                    <span class="stat-label">${type || 'None'}</span>
                    <span class="stat-value">${count.toFixed(2)}</span>
                </div>`;
        })
        .join('');
    document.getElementById('sealStatsContent').innerHTML = sealHtml;

    // Display stone count
    console.log('Processing stone count:', data.stones);
    document.getElementById('stoneStatsContent').innerHTML = `
        <div class="stat-item">
            <span class="stat-label">Average Stones</span>
            <span class="stat-value">${data.stones.toFixed(2)}</span>
        </div>`;
    
    console.log('Finished displaying all results');
} 